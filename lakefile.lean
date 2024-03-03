import Lake
open Lake DSL

require socket from git "https://github.com/hargoniX/socket.lean" @ "main"

package «http-client» where
  -- add package configuration options here

lean_lib «HttpClient» where
  -- add library configuration options here

@[default_target]
lean_exe «http-client» where
  root := `Main

target http.o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "http.o"
  let srcJob ← inputFile <| pkg.dir / "native" / "http.c"
  let weakArgs := #["-I", (← getLeanIncludeDir).toString]
  buildO "http.c" oFile srcJob weakArgs #["-fPIC"] "gcc" getLeanTrace

extern_lib libhttpclientffi pkg := do
  let name := nameToStaticLib "httpclientffi"
  let ffiO ← fetch <| pkg.target ``http.o
  buildStaticLib (pkg.nativeLibDir / name) #[ffiO]