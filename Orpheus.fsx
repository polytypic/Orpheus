// Copyright (C) by Vesa Karvonen

#I __SOURCE_DIRECTORY__

#r "Libs/Orpheus/bin/Release/FParsecCS.dll"
#r "Libs/Orpheus/bin/Release/FParsec.dll"
#r "Libs/Orpheus/bin/Release/PPrint.dll"
#r "Libs/Orpheus/bin/Release/Orpheus.dll"

open System
open Orpheus
open Orpheus.Schema

// This example is inspired by http://json-schema.org/example1.html
module ProductExample =

  type Dimensions = {
      Length: float
      Width: float
      Height: float
    }

  type Product = {
      Id: int
      Name: string
      Price: float
      Tags: list<string>
      Dimensions: option<Dimensions>
    }

  let Dimensions : Schema<Dimensions> =
    obj (lift <| fun l w h -> {Length = l; Width = w; Height = h}
     <*> req "length" (float .> 0.0)
     <*> req "width"  (float .> 0.0)
     <*> req "height" (float .> 0.0))

  let Product : Schema<Product> =
    obj (lift <| fun i n p t d ->
           {Id = i; Name = n; Price = p; Tags = t; Dimensions = d}
     <*> req "id" int
     <*> req "name" string
     <*> req "price" (float .> 0.0)
     <*> def "tags" [] (list string)
     <*> opt "dimensions" Dimensions)

  let ProductSet : Schema<list<Product>> =
    list Product

// This example is inspired by http://json-schema.org/example2.html
module MountExample =
  let regex = string // XXX
  
  type FSType = EXT3 | EXT4 | BTRFS
  type NFSServer = HostName | IPV4 | IPV6

  type Storage =
    | DiskDevice of string
    | DiskUUID of string
    | NFS of string * NFSServer
    | TMPFS of int

  type Mount = {
      Storage: Storage
      FSType: option<FSType>
      Readonly: option<bool>
      Options: list<string>
    }

  let FSType = case "ext3"  EXT3
            .|.case "ext4"  EXT4
            .|.case "btrfs" BTRFS

  let NFSServer = case "host-name" HostName
               .|.case "ipv4"      IPV4
               .|.case "ipv6"      IPV6

  let DiskDevice =
    obj (req "type" (case "disk" DiskDevice)
     <*> req "device" regex)
  let DiskUUID =
    obj (req "type" (case "disk" DiskUUID)
     <*> req "label" regex)
  let NFS =
    obj (req "type" (case "nfs" <| fun p s -> NFS (p, s))
     <*> req "remotePath" regex
     <*> req "server" NFSServer)
  let TMPFS =
    obj (req "type" (case "tmpfs" TMPFS)
     <*> req "sizeInMB" (16 <=. int .<= 512))

  let Storage = DiskDevice
             .|.DiskUUID
             .|.NFS
             .|.TMPFS

  let Mount =
    obj (lift <| fun s f r o ->
           {Storage = s; FSType = f; Readonly = r; Options = o}
     <*> req "storage" Storage
     <*> opt "fstype" FSType
     <*> opt "readonly" bool
     <*> def "options" [] (list string))
