namespace JSONs

module JSON =
  type Value =
   | Obj of Map<string, Value>
   | List of list<Value>
   | String of string
   | Float of float
   | Bool of bool
   | Null

  let mkParser () : FParsec.Primitives.Parser<Value, 's> = failwith "XXX"

  let pretty (v: Value) : PPrint.Doc = failwith "XXX"

module Spec =
  type Value<'x> = | Value

  let extract (_: Value<'x>) (_: JSON.Value) : 'x = failwith "XXX"

  let ( |>> ) (_: Value<'x>) (_: 'x -> 'y) : Value<'y> = failwith "XXX"
  let ( <|> ) (_: Value<'x>) (_: Value<'x>) : Value<'x> = failwith "XXX"

  let List (_: Value<'x>) : Value<list<'x>> = failwith "XXX"
  let String: Value<string> = Value
  let Float: Value<float> = Value
  let Bool: Value<bool> = Value
  let Null: Value<unit> = Value

  type Fields<'x> = | Fields

  let ( /> ) (_: Fields<'x>) (_: 'x -> 'y) : Fields<'y> = failwith "XXX"
  let ( <*> ) (_: Fields<'x -> 'y>) (_: Fields<'x>) : Fields<'y> = failwith "XXX"
  let ( </> ) (_: Fields<'x>) (_: Fields<unit>) : Fields<'x> = failwith "XXX"

  let con (_: 'x) : Fields<'x> = failwith "XXX"

  let req (_: string) (_: Value<'x>) : Fields<'x> = failwith "XXX"
  let opt (_: string) (_: Value<'x>) : Fields<option<'x>> = failwith "XXX"
  let def (_: string) (_: Value<'x>) (_: 'x) : Fields<'x> = failwith "XXX"

  let others: Fields<Map<string, JSON.Value>> = Fields

  let Obj (_: Fields<'x>) : Value<'x> = failwith "XXX"
