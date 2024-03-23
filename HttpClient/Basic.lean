
import Http
import Socket
import HttpClient.AddrInfo
import HttpClient.Connection

namespace HttpClient

inductive Method where
  | GET
  | POST

structure URI where
  uri : Http.URI
  hasAuth : uri.auth.isSome = true

def parseUrl (url : String) : Except String URI :=
  match Http.URI.parse.run url with
    | .ok ss r => if ss.isEmpty
      then if _ : r.auth.isSome
        then .ok {
          uri := r
          hasAuth := by assumption
        }
        else .error s!"no authority in {url}"
      else .error s!"leftofer when parsing url {url}: {ss}"
    | .error e => .error (toString e)

structure Request where
  method : Method
  uri : URI
  body : Option ByteArray

namespace Internal

opaque SSLConnection : Type

@[extern "ssl_connection_create"]
opaque SSLConnection.create : UInt32 → IO SSLConnection

@[extern "ssl_connection_connect"]
opaque SSLConnection.connect : SSLConnection → IO Unit

@[extern "ssl_connection_shutdown"]
opaque SSLConnection.shutdown : SSLConnection → IO Unit

@[extern "ssl_connection_write"]
opaque SSLConnection.write : SSLConnection → ByteArray → IO Unit

@[extern "ssl_connection_read"]
opaque SSLConnection.read : SSLConnection → UInt32 → IO ByteArray

def renderPath (uri : Http.URI) : String :=
  path ++ query
  where
  query := uri.query.map ("?" ++ toString ·) |>.getD ""
  path := if uri.path.isEmpty
    then "/"
    else uri.path.foldl (· ++ "/" ++ ·) ""

/--
Render request into a string

The standard one, `Http.Request.toRequestString`, is doing something strange with URL
 -/
def toRequestString [ToString T] (r : Http.Request T) : String :=
  s!"{r.method} {renderPath r.url} {r.version}" ++ Http.CRLF ++
  r.headers.toRequestFormat ++
  Http.CRLF ++
  toString r.body

def connect (uri : URI) : IO Connection := do
  let auth := uri.uri.auth.get uri.hasAuth
  let host := auth.host
  let secure := uri.uri.scheme == Http.URI.Scheme.HTTPS
  let service := match auth.port with
    | .none => if secure then "https" else "http"
    | .some p => toString p

  -- call getaddrinfo(3) to resolve the host
  let addrs ← AddrInfo.getAddrInfo host service
  let addrInfo ← do
    match addrs.head? with
    | none => throw (IO.userError "no address!")
    | some a => pure a

  -- connect
  let connection ← do
    let addr := match addrInfo with
      | .inet host port => Socket.SockAddr4.v4 host port
    let sock ← Socket.mk .inet .stream
    try
      sock.connect addr
      if secure
        then do
          let fd ← sock.getFd
          let sslConnection ← SSLConnection.create fd
          sslConnection.connect
          Connection.make
            sslConnection.write
            (.some <$> sslConnection.read 4096)
            (sslConnection.shutdown *> sock.close)
        else do
          Connection.make
            (λ c => sock.send c >>= λ _ => pure ())
            (.some <$> sock.recv 4096)
            sock.close
    catch e =>
      sock.close
      throw e

def runRequest (connection : Connection) (request : Request)
  : IO (Http.Response String) := do
  let uri := request.uri.uri
  let auth := uri.auth.get request.uri.hasAuth
  let host := toString auth

  -- build the request
  let request := {
    method := match request.method with
      | .GET => .GET
      | .POST => .POST
    url := {
      scheme := none
      auth := none
      path := uri.path
      query := uri.query
      fragment := uri.fragment
      : Http.URI
    }
    version := .HTTP_1_1
    headers := Http.Headers.fromList
      [ (.standard Http.HeaderName.Standard.HOST
        , host
        )
      , ( .standard Http.HeaderName.Standard.CONNECTION
        , "close"
        )
      ]
    body := match request.body with
      | .none => ""
      | .some b => String.fromUTF8Unchecked b -- XXX
    : Http.Request String
  }
  -- IO.println s!"sending request\n{toRequestString request}\nend request"

  connection.send (Internal.toRequestString request).toUTF8
  let resp ← connection.readAll
  -- IO.println s!"response:\n{String.fromUTF8Unchecked resp}\nend response"

  let responseParser := Http.Response.parse (pure ())
  match responseParser.run (String.fromUTF8Unchecked resp) with
  | .ok body resp => pure {resp with body := body.toString}
  | .error e => throw (IO.userError s!"can't parse response {e}")

end Internal
