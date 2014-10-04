// Copyright (C) by Vesa Karvonen

namespace Orpheus

type Schema<'x> = | Schema
type Fields<'x> = | Fields

exception Constraint

type Schema<'x> with
  static member (|?|) (_: Schema<'x>, _: PPrint.Doc) : Schema<'x> = Schema

  static member (|>>) (_: Schema<'x>, _: 'x -> 'y) : Schema<'y> = Schema

  static member (.&.) (_: Schema<'x>, _: Schema<'x>) : Schema<'x> = Schema
  static member (.|.) (_: Schema<'x>, _: Schema<'x>) : Schema<'x> = Schema

type Schema<'x when 'x: comparison> with
  static member (.<.) (_: 'x, _: Schema<'x>) : Schema<'x> = Schema
  static member (.<.) (_: Schema<'x>, _: 'x) : Schema<'x> = Schema
  static member (.<=.) (_: 'x, _: Schema<'x>) : Schema<'x> = Schema
  static member (.<=.) (_: Schema<'x>, _: 'x) : Schema<'x> = Schema
  static member (.>.) (_: 'x, _: Schema<'x>) : Schema<'x> = Schema
  static member (.>.) (_: Schema<'x>, _: 'x) : Schema<'x> = Schema
  static member (.>=.) (_: 'x, _: Schema<'x>) : Schema<'x> = Schema
  static member (.>=.) (_: Schema<'x>, _: 'x) : Schema<'x> = Schema

type Schema<'x when 'x: equality> with
  static member (.<>.) (_: 'x, _: Schema<'x>) : Schema<'x> = Schema
  static member (.<>.) (_: Schema<'x>, _: 'x) : Schema<'x> = Schema
  static member (.=.) (_: 'x, _: Schema<'x>) : Schema<'x> = Schema
  static member (.=.) (_: Schema<'x>, _: 'x) : Schema<'x> = Schema

module Schema =
  let explain (_: Schema<'x>) : PPrint.Doc = failwith "XXX"
  let extract (_: Schema<'x>) (_: JSON) : 'x = failwith "XXX"

  let JSON: Schema<JSON> = Schema

  let List (_: Schema<'x>) : Schema<list<'x>> = Schema
  let String: Schema<string> = Schema
  let Number: Schema<string> = Schema
  let Bool: Schema<bool> = Schema
  let Null: Schema<unit> = Schema

  let lift (_: 'x) : Fields<'x> = Fields
  let (</>) (_: Fields<'x>) (_: Fields<unit>) : Fields<'x> = Fields
  let (<*>) (_: Fields<'x -> 'y>) (_: Fields<'x>) : Fields<'y> = Fields

  let required (_: string) (_: Schema<'x>) : Fields<'x> = Fields
  let optional (_: string) (_: Schema<'x>) : Fields<option<'x>> = Fields
  let defaults (_: string) (_: Schema<'x>) (_: 'x) : Fields<'x> = Fields

  let others: Fields<Map<string, JSON>> = Fields

  let Object (_: Fields<'x>) : Schema<'x> = Schema
