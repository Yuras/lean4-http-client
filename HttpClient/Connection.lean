
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

@[simp]
axiom mk_get {T M : Type} {a : T} {x : IO.Ref T → T → IO M}: (do
  let r ← IO.mkRef a
  let v ← r.get
  x r v
  )=(do
  let r ← IO.mkRef a
  x r a : IO M)

@[simp]
axiom mk_set {T M : Type} {a b : T} {x : IO.Ref T → IO M}: (do
  let r ← IO.mkRef a
  r.set b
  x r
  )=(do
  let r ← IO.mkRef b
  x r : IO M)

@[simp]
axiom mk_alone {T M : Type} {a : T} {x : IO M}: (do
  let _ ← IO.mkRef a
  x) = x

axiom mk_io_comm {T M N : Type} (a : T) (y : IO N) (x : IO.Ref T → N → IO M)
  : (do
  let r ← IO.mkRef a
  let v ← y
  x r v) = (do
  let v ← y
  let r ← IO.mkRef a
  x r v : IO M)

@[simp]
axiom modify_get_set
  {T : Type}
  (r : IO.Ref T)
  (f : T -> T)
  : r.modify f = (r.get >>= λ v => r.set (f v) : IO Unit)

theorem mk_mk_comm {T1 T2 M : Type} {a : T1} {b : T2} {x : IO.Ref T1 → IO.Ref T2 → IO M}
  : (do
  let r1 ← IO.mkRef a
  let r2 ← IO.mkRef b
  x r1 r2) = (do
  let r2 ← IO.mkRef b
  let r1 ← IO.mkRef a
  x r1 r2 : IO M) := by
    rewrite [mk_io_comm]
    rfl

theorem mk_io_comm_ {T M N K : Type} (z : IO K) (a : T) (y : IO N) (x : IO.Ref T → N → K → IO M)
  : (do
  let k ← z
  let r ← IO.mkRef a
  let v ← y
  x r v k) = (do
  let k ← z
  let v ← y
  let r ← IO.mkRef a
  x r v k)
  := by
    refine bind_congr ?h
    intro k
    exact mk_io_comm a y fun r v => x r v k

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
  simp
  rewrite [mk_mk_comm]
  simp
  rewrite [mk_mk_comm]
  simp

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
  simp

theorem receive_twice
  (send_ : ByteArray → IO Unit)
  (receive_ : IO (Option ByteArray))
  (close_ : IO Unit)
  (h : receive_ = pure .none):
  (do
    let conn ← Connection.make send_ receive_ close_
    let _ ← conn.receive
    conn.receive)
    = pure .none
  := by
  unfold Connection.make
  simp
  rewrite [mk_mk_comm]
  simp
  rewrite [h]
  simp
  rewrite [mk_mk_comm]
  simp

theorem receive
  (send_ : ByteArray → IO Unit)
  (receive_ : IO (Option ByteArray))
  (close_ : IO Unit)
  (res : Option ByteArray)
  (h1 : res.map ByteArray.isEmpty ≠ .some true)
  (h : receive_ = pure res):
  (do
    let conn ← Connection.make send_ receive_ close_
    conn.receive)
    = receive_
  := by
  unfold Connection.make
  simp
  rewrite [h]
  simp
  cases res
  simp
  simp
  simp at h1
  intro h2
  absurd h1
  simp
  assumption

theorem receive_empty
  (send_ : ByteArray → IO Unit)
  (receive_ : IO (Option ByteArray))
  (close_ : IO Unit)
  (h : receive_ = pure (.some ByteArray.empty)):
  (do
    let conn ← Connection.make send_ receive_ close_
    conn.receive)
    = pure .none
  := by
  unfold Connection.make
  simp
  rewrite [h]
  simp
  have empty : ByteArray.isEmpty ByteArray.empty := by
    unfold ByteArray.isEmpty
    simp
  rewrite [empty]
  simp

theorem receive_once
  (send_ : ByteArray → IO Unit)
  (receive_ : IO (Option ByteArray))
  (close_ : IO Unit) :
  (do
    let conn ← Connection.make send_ receive_ close_
    conn.receive)
    = (do
    let mc ← receive_
    match mc with
    |  .none => pure .none
    |  .some c =>
      if ByteArray.isEmpty c
        then pure .none
        else pure (.some c)
    )
  := by
  unfold Connection.make
  simp
