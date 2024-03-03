
namespace Internal

opaque AddrInfoList : Type

@[extern "get_addr_info"]
opaque AddrInfoList.getAddrInfo : Strign → String → IO AddrInfoList

@[extern "addr_info_list_next"]
opaque AddrInfoList.next : AddrInfoList → IO Bool

@[extern "addr_info_list_get_family"]
opaque AddrInfoList.family : AddrInfoList → UInt32

@[extern "addr_info_list_inet_get_addr"]
opaque AddrInfoList.inetAddr : AddrInfoList → UInt32

@[extern "addr_info_list_inet_get_port"]
opaque AddrInfoList.inetPort : AddrInfoList → UInt16

partial
def AddrInfoList.iterate {a : Type} (list : AddrInfoList) (f : AddrInfoList → IO a) : IO (List a) :=
  go List.nil
  where
  go res := do
    let a ← f list
    if ← list.next
      then go (List.cons a res)
      else pure (List.reverse (List.cons a res))

end Internal

inductive AddrInfo where
  | inet (host : UInt32) (port : UInt16) : AddrInfo

instance : ToString AddrInfo where
  toString addr :=
    match addr with
    | .inet host port => s!"{host}:{port}"

def AddrInfo.getAddrInfo (host : String) (port : String) : IO (List AddrInfo) := do
  let list ← Internal.AddrInfoList.getAddrInfo host port
  list.iterate fun _ => do
    match list.family with
    | 2 => pure (.inet list.inetAddr list.inetPort)
    | _ => throw $ IO.userError s!"Http.GetAddrInfo: unknown family {list.family}"
