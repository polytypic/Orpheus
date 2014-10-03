// Copyright (C) by Vesa Karvonen

namespace Orpheus

type JSON =
 | Object of Map<string, JSON>
 | List of list<JSON>
 | String of string
 | Number of string
 | Bool of bool
 | Null

[<AutoOpen>]
module Basic =
  val show: JSON -> string
  val read: string -> JSON

  val mkParser: unit -> FParsec.Primitives.Parser<JSON, 's>
  val pretty: JSON -> PPrint.Doc
