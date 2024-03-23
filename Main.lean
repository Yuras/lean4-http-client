import HttpClient


def main : IO Unit := do
  let resp ← HttpClient.http .GET "http://localhost:8080" .none
  -- let resp ← HttpClient.http .GET "http://www.google.com" .none
  IO.println s!"{resp.toString}"
