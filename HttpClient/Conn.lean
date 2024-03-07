
import Mathlib

namespace Local

structure Conn where
  receive : IO (Option ByteArray)
  push : ByteArray -> IO Unit

def mkRef {T : Type} (a : T) : IO (IO.Ref T):= do
  IO.mkRef a

def Conn.make (receive_ : IO (Option ByteArray)) : IO Conn := do
  let memRef ← mkRef []
  pure {
    receive := do
      match ← memRef.get with
        | .nil => receive_
        | .cons chunk rest => do
          memRef.set rest
          pure (.some chunk)
    push := λ chunk => do
      let l ← memRef.get
      memRef.set (chunk :: l)
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

theorem push_receive
  (receive_ : IO (Option ByteArray))
  (chunk : ByteArray) :
  (Conn.make receive_ >>= λ conn => conn.push chunk >>= λ _ => conn.receive)
    = pure (.some chunk)
  := by
  unfold Conn.make
  simp [mk_get, mk_set, mk_alone]

end Local
