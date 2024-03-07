HTTP client for Lean4

Status: not designed for any serious use, at least not yet

Example

```lean4
import «HttpClient»

def main : IO Unit := do
  let response ← HttpClient.get "http://localhost:8080/test"
  IO.println s!"{response}"
```

For more control, use `HttpClient.runRequest`.
