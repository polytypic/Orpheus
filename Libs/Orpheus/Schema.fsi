// Copyright (C) by Vesa Karvonen

namespace Orpheus

type Schema<'x>
type Fields<'x>

module Schema =
  val explain: Schema<'x> -> PPrint.Doc
  val extract: Schema<'x> -> JSON -> 'x

  val (|>>): Schema<'x> -> ('x -> 'y) -> Schema<'y>
  val (<|>): Schema<'x> -> Schema<'x> -> Schema<'x>
  val (<?>): Schema<'x> -> string -> Schema<'x>

  val JSON: Schema<JSON>

  val List: Schema<'x> -> Schema<list<'x>>
  val String: Schema<string>
  val Number: Schema<string>
  val Bool: Schema<bool>
  val Null: Schema<unit>

  val (/>): Fields<'x> -> ('x -> 'y) -> Fields<'y>
  val (<*>): Fields<'x -> 'y> -> Fields<'x> -> Fields<'y>
  val (</>): Fields<'x> -> Fields<unit> -> Fields<'x>

  val lift: 'x -> Fields<'x>

  val required: string -> Schema<'x> -> Fields<'x>
  val optional: string -> Schema<'x> -> Fields<option<'x>>
  val defaults: string -> Schema<'x> -> 'x -> Fields<'x>

  val others: Fields<Map<string, JSON>>

  val Object: Fields<'x> -> Schema<'x>
