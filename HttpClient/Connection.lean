
structure Connection where
  send : ByteArray -> IO Unit
  receive : IO (Option ByteArray)
  push : ByteArray -> IO Unit
  close : IO Unit

def Connection.make
  (send_ : ByteArray -> IO Unit)
  (receive_ : IO (Option ByteArray))
  (close_ : IO Unit)
  : IO Connection := do
  let memRef ← IO.mkRef []
  let closedRef ← IO.mkRef false
  let checkClosed : IO Unit := do
    if ← closedRef.get
      then throw (IO.userError "connection closed")
      else pure ()
  pure {
    send := λ chunk => do
      checkClosed
      send_ chunk
    receive := do
      checkClosed
      match ← memRef.get with
        | .nil => do
          match ← receive_ with
          | .none => pure .none
          | .some chunk =>
            if chunk.isEmpty
              then pure .none
              else pure (.some chunk)
        | .cons chunk rest => do
          memRef.set rest
          pure (.some chunk)
    push := λ chunk => do
      checkClosed
      memRef.modify λ l => .cons chunk l
    close := do
      if ← closedRef.get
        then pure ()
        else do
          closedRef.set True
          close_
    : Connection
  }

def Connectin.makeFromList (list : List ByteArray) : IO Connection := do
  let ref ← IO.mkRef list
  let send _ := pure ()
  let receive := match ← ref.get with
    | .nil => pure .none
    | .cons chunk rest => do
      ref.set rest
      pure (.some chunk)
  Connection.make send receive (pure ())

partial def Connection.readAll (c : Connection) : IO ByteArray := go []
  where
  go res := do
    match ← c.receive with
    | .none => pure (res.reverse.foldl (fun a b => a.append b) ByteArray.empty)
    | .some chunk => go (chunk :: res)
