import Lake
open Lake DSL

require Http from git "https://github.com/Yuras/lean4-http" @ "http-client"
require socket from git "https://github.com/Yuras/socket.lean.git" @ "wip/yuras/get_fd"
require soda from git "https://github.com/algebraic-sofia/soda" @ "main"
require mathlib from git "https://github.com/leanprover-community/mathlib4" @ "stable"

package «http-client» where
  -- add package configuration options here

lean_lib «HttpClient» where
  -- add library configuration options here

@[default_target]
lean_exe «http-client-main» where
  root := `Main
  moreLinkArgs := #["-lssl"]

lean_exe «http-client-spec» where
  root := `Spec
  moreLinkArgs := #["-lssl"]

target http.o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "http.o"
  let srcJob ← inputFile <| pkg.dir / "native" / "http.c"
  let weakArgs := #["-I", (← getLeanIncludeDir).toString]
  buildO "http.c" oFile srcJob weakArgs #["-fPIC"] "gcc" getLeanTrace

extern_lib libhttpclientffi pkg := do
  let name := nameToStaticLib "httpclientffi"
  let ffiO ← fetch <| pkg.target ``http.o
  buildStaticLib (pkg.nativeLibDir / name) #[ffiO]
