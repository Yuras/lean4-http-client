
import «Http»
import «Socket»
import «HttpClient».AddrInfo
import «HttpClient».Connection

namespace HttpClient

inductive Method where
  | GET
  | POST

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

def connect (uri : URI) : IO Connection := do
  let auth := uri.uri.auth.get uri.hasAuth
  let host := auth.host
  let service := match auth.port with
    | .none => "http"
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
      Connection.make
        (λ c => sock.send c >>= λ _ => pure ())
        (.some <$> sock.recv 4096)
        sock.close
    catch e =>
      sock.close
      throw e

def method' (connection : Connection) (method : Method) (uri : URI) (body : Option ByteArray) : IO (Http.Response String) := do
  let auth := uri.uri.auth.get uri.hasAuth
  let host := toString auth

  -- build the request
  let request := {
    method := match method with
      | .GET => .GET
      | .POST => .POST
    url := {
      scheme := none
      auth := none
      path := uri.uri.path
      query := uri.uri.query
      fragment := uri.uri.fragment
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
    body := match body with
      | .none => ""
      | .some b => String.fromUTF8Unchecked b -- XXX
    : Http.Request String
  }
  -- IO.println s!"sending request\n{toRequestString request}\nend request"

  connection.send (toRequestString request).toUTF8
  let resp ← connection.readAll
  -- IO.println s!"response:\n{String.fromUTF8Unchecked resp}\nend response"

  let responseParser := Http.Response.parse (pure ())
  match responseParser.run (String.fromUTF8Unchecked resp) with
  | .ok body resp => pure {resp with body := body.toString}
  | .error e => throw (IO.userError s!"can't parse response {e}")

def method (method : Method) (url : String) (body : Option ByteArray) : IO (Http.Response String) := do
  let uri ← match parseUrl url with
    | .ok res => pure res
    | .error err => throw (IO.userError err)
  let connection ← connect uri
  try
    method' connection method uri body
  finally
    connection.close

def get (url : String) : IO String := do
  let response ← method .GET url .none
  pure (response.body)

def post (url : String) (body : ByteArray) : IO String := do
  let response ← method .POST url body
  pure (response.body)

end HttpClient
