
import «Http»
import «Socket»
import «HttpClient».AddrInfo

namespace HttpClient

inductive Method where
  | GET
  | POST

def renderPath (uri : Http.URI) : String :=
  if uri.path.isEmpty
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

/-- Read everything from the socket til the EOF -/
partial
def readAll (s : Socket) : IO ByteArray := go List.nil
  where
  go res := do
    let r ← s.recv 4096
    if r.size == 0
      then return (res.reverse.foldl (fun a b => a.append b) ByteArray.empty)
      else do
        let str := r
        go (List.cons str res)

def method (method : Method) (url : String) (body : Option ByteArray) : IO ByteArray := do
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

  -- create socket
  let sock ← Socket.mk .inet .stream
  let addr := match addrInfo with
    | .inet host port => Socket.SockAddr4.v4 host port

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

  -- IO.println s!"sending request\n{toRequestString request}\nend request"

  -- send it
  let resp ←
    try
      sock.connect addr
      let _ ← sock.send (toRequestString request).toUTF8
      let blocks ← readAll sock
      pure blocks
    finally
      -- I beleive I don't need this?
      sock.close

  -- IO.println s!"response:\n{String.fromUTF8Unchecked resp}\nend response"
  pure resp

def get (url : String) : IO ByteArray := do
  method .GET url .none

def post (url : String) (body : ByteArray) : IO ByteArray := do
  method .POST url body

end HttpClient
