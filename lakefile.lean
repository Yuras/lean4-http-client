import Lake
open Lake DSL

package «http-client» where
  -- add package configuration options here

lean_lib «HttpClient» where
  -- add library configuration options here

@[default_target]
lean_exe «http-client» where
  root := `Main
