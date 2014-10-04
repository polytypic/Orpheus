// Copyright (C) by Vesa Karvonen

namespace Orpheus

type JSON =
 | Obj of Map<string, JSON>
 | List of list<JSON>
 | String of string
 | Number of string
 | Bool of bool
 | Nil

[<AutoOpen>]
module Basic =
  val show: JSON -> string
  val read: string -> JSON

  val pretty: JSON -> PPrint.Doc
  val mkParser: unit -> FParsec.Primitives.Parser<JSON, 's>
