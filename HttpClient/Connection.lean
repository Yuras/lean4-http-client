
import Mathlib

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
      let l ← memRef.get
      memRef.set (chunk :: l)
      -- memRef.modify λ l => .cons chunk l
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
  let receive := do
    match ← ref.get with
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

#eval unsafeIO $ do
  let c ← Connection.make (λ _ => pure ()) (pure .none) (pure ())
  c.receive
#eval unsafeIO $ do
  let c ← Connection.make (λ _ => pure ()) (pure .none) (pure ())
  c.push "hello".toUTF8
  let res1 ← c.receive
  let res2 ← c.receive
  pure ([res1, res2].map (Option.map String.fromUTF8Unchecked))

def mkRef {T : Type} (a : T) : IO (IO.Ref T):= do
  IO.mkRef a

theorem lift_mk : liftM (IO.mkRef a) = mkRef a := by
  unfold mkRef
  rfl

axiom mk_get {T M : Type} {a : T} {x : IO.Ref T → T → IO M}: (do
  let r ← mkRef a
  let v ← r.get
  x r v
  )=(do
  let r ← mkRef a
  x r a)

axiom mk_set {T M : Type} {a b : T} {x : IO.Ref T → IO M}: (do
  let r ← mkRef a
  r.set b
  x r
  )=(do
  let r ← mkRef b
  x r)

axiom mk_alone {T M : Type} {a : T} {x : IO M}: (do
  let _ ← mkRef a
  x) = x

axiom mk_IO_comm {T1 T2 M N : Type} {a : T1} {b : T2} {y : IO N} {x : IO.Ref T1 → N → IO M}
  : (do
  let r ← mkRef a
  let v ← y
  x r v) = (do
  let v ← y
  let r ← mkRef a
  x r v)

theorem mk_mk_comm {T1 T2 M : Type} {a : T1} {b : T2} {x : IO.Ref T1 → IO.Ref T2 → IO M}
  : (do
  let r1 ← mkRef a
  let r2 ← mkRef b
  x r1 r2) = (do
  let r2 ← mkRef b
  let r1 ← mkRef a
  x r1 r2) := by
    rewrite [@mk_IO_comm _ _ _ _ _ b]
    rfl

theorem push_receive
  (send_ : ByteArray → IO Unit)
  (receive_ : IO (Option ByteArray))
  (close_ : IO Unit)
  (chunk : ByteArray) :
  (do
    let conn ← Connection.make send_ receive_ close_
    conn.push chunk
    conn.receive)
    = pure (.some chunk)
  := by
  unfold Connection.make
  simp [mk_get, mk_set, mk_alone, lift_mk]
  rewrite [mk_mk_comm]
  simp [mk_get, mk_set, mk_alone]
  rewrite [mk_mk_comm]
  simp [mk_get, mk_set, mk_alone]

theorem close
  (send_ : ByteArray → IO Unit)
  (receive_ : IO (Option ByteArray))
  (close_ : IO Unit):
  (do
    let conn ← Connection.make send_ receive_ close_
    conn.close)
    = close_
  := by
  unfold Connection.make
  simp [mk_get, mk_set, mk_alone, lift_mk]
