import «HttpClient»

def main : IO Unit := do
  let rr ← HttpClient.get "http://localhost:8080/test/1"
  IO.println s!"{String.fromUTF8Unchecked rr}"
