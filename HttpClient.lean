import «HttpClient».Basic

namespace HttpClient

def runRequest (request : Request) : IO (Http.Response String) := do
  let connection ← Internal.connect request.uri
  try
    Internal.runRequest connection request
  finally
    connection.close

def http (method : Method) (url : String) (body : Option ByteArray)
  : IO (Http.Response String) := do
  let uri ← match parseUrl url with
    | .ok res => pure res
    | .error err => throw (IO.userError err)
  let request := {
    method := method
    uri := uri
    body := body
    : Request
  }
  runRequest request

def get (url : String) : IO String := do
  let response ← http .GET url .none
  pure (response.body)

def post (url : String) (body : ByteArray) : IO String := do
  let response ← http .POST url body
  pure (response.body)

end HttpClient
