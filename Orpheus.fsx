// Copyright (C) by Vesa Karvonen

#I __SOURCE_DIRECTORY__

#r "Libs/Orpheus/bin/Release/FParsecCS.dll"
#r "Libs/Orpheus/bin/Release/FParsec.dll"
#r "Libs/Orpheus/bin/Release/PPrint.dll"
#r "Libs/Orpheus/bin/Release/Orpheus.dll"

open System
open Orpheus
open Orpheus.Schema

[<AutoOpen>]
module Defs =
  open PPrint

  let Float = Number |>> float |?| txt "float"
  let Int = Number |>> int |?| txt "int"

  let Case (s: string) (c: 'c) : Schema<'c> = String .=. s |>> fun _ -> c

  let Regex = String // XXX

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
    Object (lift <| fun l w h -> {Length = l; Width = w; Height = h}
        <*> required "length" (Float .>. 0.0)
        <*> required "width"  (Float .>. 0.0)
        <*> required "height" (Float .>. 0.0))

  let Product : Schema<Product> =
    Object (lift <| fun i n p t d ->
              {Id = i; Name = n; Price = p; Tags = t; Dimensions = d}
       <*> required "id" Int
       <*> required "name" String
       <*> required "price" (Float .>. 0.0)
       <*> defaults "tags" (List String) []
       <*> optional "dimensions" Dimensions)

  let ProductSet : Schema<list<Product>> =
    List Product

// This example is inspired by http://json-schema.org/example2.html
module MountExample =
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

  let FSType = Case "ext3"  EXT3
            .|.Case "ext4"  EXT4
            .|.Case "btrfs" BTRFS

  let NFSServer = Case "host-name" HostName
               .|.Case "ipv4"      IPV4
               .|.Case "ipv6"      IPV6

  let DiskDevice =
    Object (required "type" (Case "disk" DiskDevice)
        <*> required "device" Regex)
  let DiskUUID =
    Object (required "type" (Case "disk" DiskUUID)
        <*> required "label" Regex)
  let NFS =
    Object (required "type" (Case "nfs" <| fun p s -> NFS (p, s))
        <*> required "remotePath" Regex
        <*> required "server" NFSServer)
  let TMPFS =
    Object (required "type" (Case "tmpfs" TMPFS)
        <*> required "sizeInMB" (16 .<=. Int .<=. 512))

  let Storage = DiskDevice
             .|.DiskUUID
             .|.NFS
             .|.TMPFS

  let Mount =
    Object (lift <| fun s f r o ->
              {Storage = s; FSType = f; Readonly = r; Options = o}
        <*> required "storage" Storage
        <*> optional "fstype" FSType
        <*> optional "readonly" Bool
        <*> defaults "options" (List String) [])
