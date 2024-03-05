import «HttpClient».Connection

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
  /- The only different between the following two cases
  is `do` in the definition of `action`. Both cases
  compile just fine, but produce different results -/
  case "ref with do" $ do
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
  case "ref without do" $ do
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
  case "receive returns chunks one by one" $ do
    let c ← Connectin.makeFromList (["AB", "C"].map String.toUTF8)
    let r1 ← c.receive
    let r2 ← c.receive
    let r3 ← c.receive
    pure (r1.map String.fromUTF8Unchecked == "AB"
      && r2.map String.fromUTF8Unchecked == "C"
      && r3.isNone)

def main : IO Unit := do
  refSpec
  connectionSpec
