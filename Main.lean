import «HttpClient»

def main : IO Unit := do
  let resp ← HttpClient.http .GET "http://localhost:8080" .none
  IO.println s!"{resp.toString}"
