import «HttpClient»
import «HttpClient».Connection
import «HttpClient».Conn
import «Soda»

def main : IO Unit := do
  let resp ← HttpClient.method .GET "http://localhost:8080" .none
  IO.println s!"{resp.toString}"
