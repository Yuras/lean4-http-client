import «Http»
import «HttpClient»
import «HttpClient».Connection

def List.concatenate [Append a] [Inhabited a] (l : List a) : a :=
  l.foldl (. ++ .) default

def case (name : String) (f : IO Bool) : IO Unit := do
  println! "{name}:"
  let res ← try
    if ← f
      then pure "OK"
      else pure "FAIL"
  catch e => do
    pure s!"EXCEPTION {e.toString}"
  println! "\t{res}"

def refSpec : IO Unit := do
  /- The only difference between the following two cases
  is `do` in the definition of `action`. Both cases
  compile just fine, but produce different results -/
  case "ref with do" do
    let ref ← IO.mkRef true
    let action := do
      if ← ref.get
        then do
          ref.set false
          pure true
        else pure false
    let r1 ← action
    let r2 ← action
    pure ([r1, r2] == [true, false])
  case "ref without do" do
    let ref ← IO.mkRef true
    let action :=
      if ← ref.get
        then do
          ref.set false
          pure true
        else pure false
    let r1 ← action
    let r2 ← action
    pure ([r1, r2] == [true, true])

def connectionSpec : IO Unit := do
  case "receive returns chunks one by one" do
    let c ← Connection.makeFromList (["AB", "C"].map String.toUTF8)
    let r1 ← c.receive
    let r2 ← c.receive
    let r3 ← c.receive
    pure (r1.map String.fromUTF8Unchecked == "AB"
      && r2.map String.fromUTF8Unchecked == "C"
      && r3.isNone)
  case "push and receive gives back the pushed value" do
    let c ← Connection.makeFromList (["AB", "C"].map String.toUTF8)
    c.push "DE".toUTF8
    let r ← c.receive
    pure (r.map String.fromUTF8Unchecked == "DE")
  case "listenner returns everything sent so far" do
    let (c, listener) ← do
      let c ← Connection.makeFromList (["AB", "C"].map String.toUTF8)
      c.listener
    c.send "AB".toUTF8
    c.send "C".toUTF8
    let r ← listener
    pure (r.map String.fromUTF8Unchecked == ["AB", "C"])

def methodSpec : IO Unit := do
  let emptyResponse := "HTTP/1.1 200 OK\n\n".toUTF8
  let url := "http://localhost:80"
  let uri ← match HttpClient.parseUrl url with
    | .ok res => pure res
    | .error err => throw (IO.userError err)
  case "sends correct request" do
    let expected := "GET / HTTP/1.1\r\nhost: localhost\r\nconnection: close\r\n\r\n"
    let (c, listener) ← do
      let c ← Connection.makeFromList [emptyResponse]
      c.listener
    let _ ← HttpClient.method' c .GET uri .none
    let sent ← listener
    pure (String.fromUTF8Unchecked sent.concatenate == expected)

def main : IO Unit := do
  refSpec
  connectionSpec
  methodSpec
