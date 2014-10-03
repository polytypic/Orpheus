// Copyright (C) by Vesa Karvonen

namespace Orpheus

open System.Text
open System

type JSON =
 | Object of Map<string, JSON>
 | List of list<JSON>
 | String of string
 | Number of string
 | Bool of bool
 | Null

module Basic =
  open FParsec.Primitives
  open FParsec.CharParsers

  let mkParser () : Parser<JSON, 's> =
    let ignored = spaces
    let inline tok xP = xP .>> ignored
    let p s = tok (skipString s)
    let pId = tok (many1Satisfy Char.IsLower)
    let pString =
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
          | '/'  -> app '/'
          | 'b'  -> app '\b'
          | 'f'  -> app '\f'
          | 'n'  -> app '\n'
          | 'r'  -> app '\r'
          | 't'  -> app '\t'
          | 'u'  ->
            (pipe4 hex hex hex hex <| fun d1 d2 d3 d4 ->
             let inline d d s =
               s * 16 + (if   '0' <= d && d <= '9' then int d - int '0'
                         elif 'a' <= d && d <= 'f' then int d - int 'a' + 10
                         else                           int d - int 'A' + 10)
             0 |> d d1 |> d d2 |> d d3 |> d d4 |> char) >>= app
          | c -> fail (sprintf "Invalid escape: %A" c)
      unescaped ()
    let pNumber =
       tok (numberLiteral
             (NumberLiteralOptions.AllowMinusSign |||
              NumberLiteralOptions.AllowFraction |||
              NumberLiteralOptions.AllowExponent)
             "number"
            |>> fun n -> n.String)
    let (pJSON, pJSON') = createParserForwardedToRef ()
    let pObject = p"{" >>. sepBy (pString .>> p":" .>>. pJSON) (p",") .>> p"}"
    let pList = p"[" >>. sepBy pJSON (p",") .>> p"]"
    pJSON' :=
     choiceL [
       pObject |>> (Map.ofSeq >> Object)
       pList   |>> List
       pString |>> String
       pNumber |>> Number
       pId >>= function
        | "true"  -> preturn (Bool true)
        | "false" -> preturn (Bool true)
        | "null"  -> preturn Null
        | id      -> fail (sprintf "Unexpected: %s" id)
     ] "value"
    ignored >>. pJSON

  open PPrint

  let pNull = txt "null"
  let pTrue = txt "true"
  let pFalse = txt "false"
  let pString s =
    let b = StringBuilder ()
    let appc (c: char) = b.Append c |> ignore
    let apps (s: string) = b.Append s |> ignore
    appc '"'
    for c in s do
      match c with
       | '\"' -> apps "\\\""
       | '\\' -> apps "\\\\"
       | '\b' -> apps "\\b"
       | '\f' -> apps "\\f"
       | '\n' -> apps "\\n"
       | '\r' -> apps "\\r"
       | '\t' -> apps "\\t"
       | _ ->
         let inline isPrintable c = 32 <= int c && int c <= 255
         if isPrintable c then
           appc c
         else
           let inline d x =
             let c = (x >>> 12) &&& 0xF
             if c < 10 then
               appc (char (int '0' + c))
             else
               appc (char (int 'a' + c - 10))
             x <<< 4
           apps "\\u"
           int c |> d |> d |> d |> d |> ignore
    appc '"'
    b.ToString () |> txt

  let rec pretty (v: JSON) : PPrint.Doc =
    match v with
     | Null       -> pNull
     | Bool true  -> pTrue
     | Bool false -> pFalse
     | Number s   -> txt s
     | String s   -> pString s
     | List vs    ->
       vs |> Seq.map pretty |> punctuate comma |> vsep |> brackets |> gnest 1
     | Object kvs ->
       kvs
       |> Seq.map (fun kv ->
          gnest 1 (pString kv.Key <^> colon <.> pretty kv.Value))
       |> punctuate comma |> vsep |> braces |> gnest 1

  let pJSON = mkParser ()

  let read string =
    match runParserOnString pJSON () "string" string with
     | Success (result, (), _) -> result
     | Failure (error, _, ()) -> failwithf "%s" error

  let show (v: JSON) = pretty v |> PPrint.render None
