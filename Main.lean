import «HttpClient»
import «Socket»

partial
def readLoop (s : Socket) := go List.nil
  where
  go res := do
    let r ← s.recv 4096
    if r.size == 0
      then return res
      else do
        IO.println s!"{String.fromUTF8Unchecked r}"
        go (List.cons r.size res)

def main : IO Unit := do
  let addrs ← AddrInfo.getAddrInfo "localhost" "8080"
  IO.println s!"{addrs}"
  let addrInfo ← do
    match addrs.head? with
    | none => throw (IO.userError "no address!")
    | some a => pure a

  let sock ← Socket.mk .inet .stream
  let blocks ←
    try
      let addr := match addrInfo with
        | .inet host port => Socket.SockAddr4.v4 host port
      sock.connect addr
      let _ ← sock.send "GET / HTTP/1.0\r\nHost: localhost\r\n\r\n".toUTF8
      let blocks ← readLoop sock
      pure blocks
    finally
      sock.close

  IO.println s!"{blocks}"
