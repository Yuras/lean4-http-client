
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
