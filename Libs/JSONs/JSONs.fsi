namespace JSONs

module JSON =
  type Value =
   | Obj of Map<string, Value>
   | List of list<Value>
   | String of string
   | Float of float
   | Bool of bool
   | Null

  val mkParser: unit -> FParsec.Primitives.Parser<Value, 's>

  val pretty: Value -> PPrint.Doc

module Spec =
  type [<Sealed>] Value<'x> =
    static member ( |>> ): Value<'x> * ('x -> 'y) -> Value<'y>
    static member ( <|> ): Value<'x> * Value<'x> -> Value<'x>

  val List: Value<'x> -> Value<list<'x>>
  val String: Value<string>
  val Float: Value<float>
  val Bool: Value<bool>
  val Null: Value<unit>

  type [<Sealed>] Fields<'x>

  val ( /> ): Fields<'x> -> ('x -> 'y) -> Fields<'y>
  val ( <*> ): Fields<'x -> 'y> -> Fields<'x> -> Fields<'y>
  val ( </> ): Fields<'x> -> Fields<unit> -> Fields<'x>

  val con: 'x -> Fields<'x>

  val req: string -> Value<'x> -> Fields<'x>
  val opt: string -> Value<'x> -> Fields<option<'x>>
  val def: string -> Value<'x> -> 'x -> Fields<'x>

  val others: Fields<Map<string, JSON.Value>>

  val Obj: Fields<'x> -> Value<'x>
