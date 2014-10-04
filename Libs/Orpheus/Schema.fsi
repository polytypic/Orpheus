// Copyright (C) by Vesa Karvonen

namespace Orpheus

type Schema<'x>
type Fields<'x>

exception Constraint

type Schema<'x> with
  static member (|?|): Schema<'x> * PPrint.Doc -> Schema<'x>

  static member (|>>): Schema<'x> * ('x -> 'y) -> Schema<'y>

  static member (.&.): Schema<'x> * Schema<'x> -> Schema<'x>
  static member (.|.): Schema<'x> * Schema<'x> -> Schema<'x>

type Schema<'x when 'x: comparison> with
  static member (.<.): 'x * Schema<'x> -> Schema<'x>
  static member (.<.): Schema<'x> * 'x -> Schema<'x>
  static member (.<=.): 'x * Schema<'x> -> Schema<'x>
  static member (.<=.): Schema<'x> * 'x -> Schema<'x>
  static member (.>.): 'x * Schema<'x> -> Schema<'x>
  static member (.>.): Schema<'x> * 'x -> Schema<'x>
  static member (.>=.): 'x * Schema<'x> -> Schema<'x>
  static member (.>=.): Schema<'x> * 'x -> Schema<'x>

type Schema<'x when 'x: equality> with
  static member (.<>.): 'x * Schema<'x> -> Schema<'x>
  static member (.<>.): Schema<'x> * 'x -> Schema<'x>
  static member (.=.): 'x * Schema<'x> -> Schema<'x>
  static member (.=.): Schema<'x> * 'x -> Schema<'x>

module Schema =
  val explain: Schema<'x> -> PPrint.Doc
  val extract: Schema<'x> -> JSON -> 'x

  val JSON: Schema<JSON>

  val List: Schema<'x> -> Schema<list<'x>>
  val String: Schema<string>
  val Number: Schema<string>
  val Bool: Schema<bool>
  val Null: Schema<unit>

  val lift: 'x -> Fields<'x>
  val (</>): Fields<'x> -> Fields<unit> -> Fields<'x>
  val (<*>): Fields<'x -> 'y> -> Fields<'x> -> Fields<'y>

  val required: string -> Schema<'x> -> Fields<'x>
  val optional: string -> Schema<'x> -> Fields<option<'x>>
  val defaults: string -> Schema<'x> -> 'x -> Fields<'x>

  val others: Fields<Map<string, JSON>>

  val Object: Fields<'x> -> Schema<'x>
