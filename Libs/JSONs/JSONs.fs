namespace JSONs

open System.Text
open System

module JSON =
  open FParsec.Primitives
  open FParsec.CharParsers
  
  type Value =
   | Obj of Map<string, Value>
   | List of list<Value>
   | String of string
   | Float of float
   | Bool of bool
   | Null

  let mkParser () : Parser<Value, 's> =
    let ignored = spaces
    let inline tok xP = xP .>> ignored
    let p s = tok (skipString s)
    let pId = tok (many1Satisfy Char.IsLower)
    let pStr =
      skipString "\"" >>= fun () ->
      let b = StringBuilder ()
      let app (c: char) = b.Append c |> ignore
      let rec unescaped () =
         anyChar >>= function
          | '\"' -> ignored >>% b.ToString ()
          | '\\' -> escaped ()
          | c -> app c ; unescaped ()
      and escaped () =
         let inline app (c: char) = app c ; unescaped ()
         anyChar >>= function
          | '\"' -> app '\"'
          | '\\' -> app '\\'
          | '/' -> app '/'
          | 'b' -> app '\b'
          | 'f' -> app '\f'
          | 'n' -> app '\n'
          | 'r' -> app '\r'
          | 't' -> app '\t'
          | 'u' ->
            (pipe4 hex hex hex hex <| fun d1 d2 d3 d4 ->
             let inline d d s =
               s * 16 + (if   '0' <= d && d <= '9' then int d - int '0'
                         elif 'a' <= d && d <= 'f' then int d - int 'a' + 10
                         else                           int d - int 'A' + 10)
             0 |> d d1 |> d d2 |> d d3 |> d d4 |> char) >>= app
          | c -> fail (sprintf "Invalid escape: %A" c)
      unescaped ()
    let pFlt =
       tok (numberLiteral
             (NumberLiteralOptions.AllowMinusSign
             ||| NumberLiteralOptions.AllowFraction
             ||| NumberLiteralOptions.AllowExponent)
             "number" |>> fun n -> n.String |> float)
    let (pVal, pVal') = createParserForwardedToRef ()
    let pObj = p"{" >>. sepBy (pStr .>> p":" .>>. pVal) (p",") .>> p"}"
    let pList = p"[" >>. sepBy pVal (p",") .>> p"]"
    pVal' :=
     choiceL [
       pObj  |>> (Map.ofSeq >> Obj)
       pList |>> List
       pStr  |>> String
       pFlt  |>> Float
       pId >>= function
        | "true"  -> preturn (Bool true)
        | "false" -> preturn (Bool true)
        | "null"  -> preturn Null
        | id      -> fail (sprintf "Unexpected: %s" id)
     ] "value"
    ignored >>. pVal

  let pretty (v: Value) : PPrint.Doc = failwith "XXX"

module Spec =
  type Value<'x> = | Value

  let explain (_: Value<'x>) : PPrint.Doc = failwith "XXX"
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
