import «HttpClient»
import «Http»
import «Socket»

def Http.URI.renderPath (uri : Http.URI) : String :=
  if uri.path.isEmpty
    then "/"
    else uri.path.foldl (· ++ "/" ++ ·) ""

/--
Render request into a string

The standard one, `Http.Request.toRequestString`, is doing something strange with URL
 -/
def Http.Request.toRequestStringEx [ToString T] (r : Http.Request T) : String :=
  s!"{r.method} {r.url.renderPath} {r.version}" ++ Http.CRLF ++
  r.headers.toRequestFormat ++
  Http.CRLF ++
  toString r.body

/-- Read everything from the socket til the EOF -/
partial
def readAll (s : Socket) := go List.nil
  where
  go res := do
    let r ← s.recv 4096
    if r.size == 0
      then return (res.reverse.foldl (. ++ .) "")
      else do
        let str := String.fromUTF8Unchecked r
        go (List.cons str res)

def main : IO Unit := do
  let host := "localhost"
  let port := "8080"
  let path := #["some", "where"]

  -- call getaddrinfo(3) to resolve the host
  let addrs ← AddrInfo.getAddrInfo host port
  IO.println s!"{addrs}"
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
    method := .GET
    url := {
      scheme := none
      auth := none
      path := path
      query := none
      fragment := none
      : Http.URI
    }
    version := .HTTP_1_0
    headers := Http.Headers.add .empty (.standard Http.HeaderName.Standard.HOST) host
    body := ""
    : Http.Request String
  }
  IO.println s!"sending request\n{request.toRequestStringEx}\nend request"

  -- send it
  let resp ←
    try
      sock.connect addr
      let _ ← sock.send request.toRequestStringEx.toUTF8
      let blocks ← readAll sock
      pure blocks
    finally
      -- I beleive I don't need this?
      sock.close

  IO.println s!"response:\n{resp}\nend response"
