
structure Conn where
  receive : IO (Option ByteArray)
  push : ByteArray -> IO Unit

def Conn.make (receive_ : IO (Option ByteArray)) : IO Conn := do
  let memRef ← IO.mkRef []
  pure {
    receive := do
      match ← memRef.get with
        | .nil => receive_
        | .cons chunk rest => do
          memRef.set rest
          pure (.some chunk)
    push := λ chunk => do
      memRef.modify λ l => .cons chunk l
    : Conn
  }

#eval unsafeIO $ do
  let c ← Conn.make (pure .none)
  c.receive
#eval unsafeIO $ do
  let c ← Conn.make (pure (.some "test".toUTF8))
  c.push "hello".toUTF8
  let res1 ← c.receive
  let res2 ← c.receive
  pure ([res1, res2].map (Option.map String.fromUTF8Unchecked))

theorem push_receive (receive_ : IO (Option ByteArray)) (chunk : ByteArray)
  : (Conn.make receive_ >>= λ conn => conn.push chunk >>= λ _ => conn.receive) = pure chunk := by
  sorry
