// Copyright (C) by Vesa Karvonen

namespace Orpheus

type Schema<'x> = | Schema
type Fields<'x> = | Fields

module Schema =
  let explain (_: Schema<'x>) : PPrint.Doc = failwith "XXX"
  let extract (_: Schema<'x>) (_: JSON) : 'x = failwith "XXX"

  let (|>>) (_: Schema<'x>) (_: 'x -> 'y) : Schema<'y> = failwith "XXX"
  let (<|>) (_: Schema<'x>) (_: Schema<'x>) : Schema<'x> = failwith "XXX"
  let (<?>) (_: Schema<'x>) (_: string) : Schema<'x> = failwith "XXX"

  let JSON: Schema<JSON> = Schema

  let List (_: Schema<'x>) : Schema<list<'x>> = failwith "XXX"
  let String: Schema<string> = Schema
  let Number: Schema<string> = Schema
  let Bool: Schema<bool> = Schema
  let Null: Schema<unit> = Schema

  let (/>) (_: Fields<'x>) (_: 'x -> 'y) : Fields<'y> = failwith "XXX"
  let (<*>) (_: Fields<'x -> 'y>) (_: Fields<'x>) : Fields<'y> = failwith "XXX"
  let (</>) (_: Fields<'x>) (_: Fields<unit>) : Fields<'x> = failwith "XXX"

  let lift (_: 'x) : Fields<'x> = failwith "XXX"

  let required (_: string) (_: Schema<'x>) : Fields<'x> = failwith "XXX"
  let optional (_: string) (_: Schema<'x>) : Fields<option<'x>> = failwith "XXX"
  let defaults (_: string) (_: Schema<'x>) (_: 'x) : Fields<'x> = failwith "XXX"

  let others: Fields<Map<string, JSON>> = Fields

  let Object (_: Fields<'x>) : Schema<'x> = failwith "XXX"
