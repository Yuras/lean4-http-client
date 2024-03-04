import «HttpClient»

def main : IO Unit := do
  let resp ← HttpClient.method .GET "http://localhost:8080/test" .none
  IO.println s!"{resp.toString}"
