#I __SOURCE_DIRECTORY__

#r "Libs/JSONs/bin/Release/FParsecCS.dll"
#r "Libs/JSONs/bin/Release/FParsec.dll"
#r "Libs/JSONs/bin/Release/PPrint.dll"
#r "Libs/JSONs/bin/Release/JSONs.dll"

open System
open JSONs
open JSONs.Spec

module Test =
  open FParsec.Primitives
  open FParsec.CharParsers

  let pJSON : Parser<JSON.Value, unit> = JSONs.JSON.mkParser ()
  let parseString string =
    match runParserOnString pJSON () "string" string with
     | Success (result, (), _) -> result
     | Failure (error, _, ()) -> failwithf "%s" error

// This example is inspired by http://json-schema.org/example1.html
module ProductExample =
  let gt (min: float) x = if min < x then x else failwith "gt"
  let none m = if Map.isEmpty m then () else failwith "none"

  let Int = Float |>> int

  type Dimensions = {
      length: float
      width: float
      height: float
    }

  type Product = {
      id: int
      name: string
      price: float
      tags: list<string>
      dimensions: option<Dimensions>
    }

  let Dimensions : Value<Dimensions> =
    Obj (con (fun length width height ->
                {length = length; width = width; height = height})
         <*> req "length" Float /> gt 0.0
         <*> req "width"  Float /> gt 0.0
         <*> req "height" Float /> gt 0.0)

  let Product : Value<Product> =
    Obj (con (fun id name price tags dimensions ->
                {id = id; name = name; price = price; tags = tags;
                 dimensions = dimensions})
         <*> req "id" Int
         <*> req "name" String
         <*> req "price" Float /> gt 0.0
         <*> def "tags" (List String) []
         <*> opt "dimensions" Dimensions)

  let ProductSet : Value<list<Product>> =
    List Product

// This example is inspired by http://json-schema.org/example2.html
module MountExample =
  let Enum svs =
    String |>> fun s ->
    match svs |> Seq.tryPick (fun (k, v) -> if s = k then Some v else None) with
      | None -> failwith "XXX"
      | Some v -> v

  let Int = Float |>> int
  let StringIs s = String |>> fun s' -> if s' <> s then failwith "StringIs"
  let Regex = String // XXX
  let between lo hi x = if lo <= x && x <= lo then x else failwith "between"

  type FSType = EXT3 | EXT4 | BTRFS
  let FSType : Value<FSType> =
    Enum [("ext3", EXT3)
          ("ext4", EXT4)
          ("btrfs", BTRFS)]

  type NFSServer = HostName | IPV4 | IPV6
  let NFSServer =
    Enum [("host-name", HostName)
          ("ipv4", IPV4)
          ("ipv6", IPV6)]
  type Storage =
    | DiskDevice of string
    | DiskUUID of string
    | NFS of string * NFSServer
    | TMPFS of int

  let DiskDevice' =
    Obj (con DiskDevice
         </> req "type" (StringIs "disk")
         <*> req "device" Regex)
  let DiskUUID' =
    Obj (con DiskUUID
         </> req "type" (StringIs "disk")
         <*> req "label" Regex)
  let NFS' =
    Obj (con (fun path server -> NFS (path, server))
         </> req "type" (StringIs "nfs")
         <*> req "remotePath" Regex
         <*> req "server" NFSServer)
  let TMPFS' =
    Obj (con TMPFS
         </> req "type" (StringIs "tmpfs")
         <*> req "sizeInMB" Int /> between 16 512)

  let Storage = DiskDevice' <|> DiskUUID' <|> NFS' <|> TMPFS'

  type Mount = {
      storage: Storage
      fstype: option<FSType>
      readonly: option<bool>
      options: list<string>
    }

  let Mount =
    Obj (con (fun storage fstype readonly options ->
                {storage = storage; fstype = fstype;
                 readonly = readonly; options = options})
         <*> req "storage" Storage
         <*> opt "fstype" FSType
         <*> opt "readonly" Bool
         <*> def "options" (List String) [])
