
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

def method (method : Method) (url : String) (body : Option ByteArray) : IO (Http.Response String) := do
  -- parse the URL
  let uri ← match Http.URI.parse.run url with
    | .ok ss r => if ss.isEmpty then pure r else throw (IO.userError s!"parse url: leftofer {ss}")
    | .error e => throw (IO.userError s!"parse url: {e}")

  let auth ← match uri.auth with
    | .none => throw (IO.userError s!"url without host {url}")
    | .some auth => pure auth
  let host := auth.host
  let service := match auth.port with
    | .none => "http"
    | .some p => toString p

  -- call getaddrinfo(3) to resolve the host
  let addrs ← AddrInfo.getAddrInfo host service
  -- IO.println s!"{addrs}"
  let addrInfo ← do
    match addrs.head? with
    | none => throw (IO.userError "no address!")
    | some a => pure a

  -- build the request
  let request := {
    method := match method with
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
    version := .HTTP_1_0
    headers := Http.Headers.add .empty (.standard Http.HeaderName.Standard.HOST) host
    body := match body with
      | .none => ""
      | .some b => String.fromUTF8Unchecked b -- XXX
    : Http.Request String
  }
  IO.println s!"sending request\n{toRequestString request}\nend request"

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

  -- send the request
  let resp ←
    try
      connection.send (toRequestString request).toUTF8
      connection.readAll
    finally
      connection.close
  -- IO.println s!"response:\n{String.fromUTF8Unchecked resp}\nend response"

  let responseParser := Http.Response.parse (pure ())
  match responseParser.run (String.fromUTF8Unchecked resp) with
  | .ok body resp => pure {resp with body := body.toString}
  | .error e => throw (IO.userError s!"can't parse response {e}")

def get (url : String) : IO String := do
  let response ← method .GET url .none
  pure (response.body)

def post (url : String) (body : ByteArray) : IO String := do
  let response ← method .POST url body
  pure (response.body)

end HttpClient
