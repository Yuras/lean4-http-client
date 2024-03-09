
import Mathlib.Init.Control.Lawful
import HttpClient.Connection
import HttpClient.IORef

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
  rfl

theorem receive_comm
  (send_ : ByteArray → IO Unit)
  (receive_ : IO (Option ByteArray))
  (close_ : IO Unit)
  (x : IO T)
  (y : Connection → T → IO R):
  (do
    let conn ← Connection.make send_ receive_ close_
    let v ← x
    y conn v)
  = (do
    let v ← x
    let conn ← Connection.make send_ receive_ close_
    y conn v)
  := by
    unfold Connection.make
    simp
    rewrite [mk_io_comm_]
    rewrite [mk_io_comm]
    simp
