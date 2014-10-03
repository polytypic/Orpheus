// Copyright (C) by Vesa Karvonen

#I __SOURCE_DIRECTORY__

#r "Libs/JSONs/bin/Release/FParsecCS.dll"
#r "Libs/JSONs/bin/Release/FParsec.dll"
#r "Libs/JSONs/bin/Release/PPrint.dll"
#r "Libs/JSONs/bin/Release/JSONs.dll"

open System
open Orpheus
open Orpheus.Schema

// This example is inspired by http://json-schema.org/example1.html
module ProductExample =
  let gt (min: float) x = if min < x then x else failwith "gt"
  let none m = if Map.isEmpty m then () else failwith "none"
  let Float = Number |>> float

  let Int = Number |>> int

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

  let Dimensions : Schema<Dimensions> =
    Object (lift <| fun length width height ->
              {length = length; width = width; height = height}
       <*> required "length" Float /> gt 0.0
       <*> required "width"  Float /> gt 0.0
       <*> required "height" Float /> gt 0.0)

  let Product : Schema<Product> =
    Object (lift <| fun id name price tags dimensions ->
              {id = id; name = name; price = price; tags = tags;
               dimensions = dimensions}
       <*> required "id" Int
       <*> required "name" String
       <*> required "price" Float /> gt 0.0
       <*> defaults "tags" (List String) []
       <*> optional "dimensions" Dimensions)

  let ProductSet : Schema<list<Product>> =
    List Product

// This example is inspired by http://json-schema.org/example2.html
module MountExample =
  let (=?) xS x = xS |>> fun x' -> if x' = x then x else failwith "/?"
  let (=%) xS y = xS |>> fun _ -> y
  let Case s c = String =? s =% c

  let Int = Number |>> int
  let Regex = String // XXX
  let between lo hi x = if lo <= x && x <= lo then x else failwith "between"

  type FSType = EXT3 | EXT4 | BTRFS
  let FSType =  Case "ext3"  EXT3
            <|> Case "ext4"  EXT4
            <|> Case "btrfs" BTRFS

  type NFSServer = HostName | IPV4 | IPV6
  let NFSServer =  Case "host-name" HostName
               <|> Case "ipv4"      IPV4
               <|> Case "ipv6"      IPV6

  type Storage =
    | DiskDevice of string
    | DiskUUID of string
    | NFS of string * NFSServer
    | TMPFS of int

  let DiskDevice' =
    Object (lift DiskDevice
        </> required "type" (Case "disk" ())
        <*> required "device" Regex)
  let DiskUUID' =
    Object (lift DiskUUID
        </> required "type" (Case "disk" ())
        <*> required "label" Regex)
  let NFS' =
    Object (lift <| fun path server -> NFS (path, server)
        </> required "type" (Case "nfs" ())
        <*> required "remotePath" Regex
        <*> required "server" NFSServer)
  let TMPFS' =
    Object (lift TMPFS
        </> required "type" (Case "tmpfs" ())
        <*> required "sizeInMB" Int /> between 16 512)

  let Storage =  DiskDevice'
             <|> DiskUUID'
             <|> NFS'
             <|> TMPFS'

  type Mount = {
      storage: Storage
      fstype: option<FSType>
      readonly: option<bool>
      options: list<string>
    }

  let Mount =
    Object (lift <| fun storage fstype readonly options ->
              {storage = storage; fstype = fstype;
               readonly = readonly; options = options}
        <*> required "storage" Storage
        <*> optional "fstype" FSType
        <*> optional "readonly" Bool
        <*> defaults "options" (List String) [])
