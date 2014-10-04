// Copyright (C) by Vesa Karvonen

namespace Orpheus

open System.Text
open System

type JSON =
 | Obj of Map<string, JSON>
 | List of list<JSON>
 | String of string
 | Number of string
 | Bool of bool
 | Nil

module Basic =
  open FParsec.Primitives
  open FParsec.CharParsers

  let t = Bool true
  let f = Bool false

  let inline toMap kvs =
    List.fold
     (fun m (k, v) ->
        if Map.containsKey k m
        then failwithf "duplicate key: \"%s\"" k
        else Map.add k v m)
     Map.empty
     kvs

  let inline p c = skipChar c .>> spaces

  let mkParser () =
    let pId = many1Satisfy Char.IsLower .>> spaces
    let pString =
      skipChar '\"' >>= fun () ->
      let b = StringBuilder ()
      let inline app (c: char) = b.Append c |> ignore
      let rec unescaped () =
        anyChar >>= function
         | '\"' -> spaces >>% b.ToString ()
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
         | c -> sprintf "Invalid escape: %A" c |> fail
      unescaped ()
    let pNumber =
      numberLiteral
       (NumberLiteralOptions.AllowMinusSign |||
        NumberLiteralOptions.AllowFraction |||
        NumberLiteralOptions.AllowExponent)
       "number" |>> (fun n -> n.String) .>> spaces
    let (pJSON, pJSON') = createParserForwardedToRef ()
    let pObj = p '{' >>. sepBy (pString .>> p ':' .>>. pJSON) (p ',') .>> p '}'
    let pList = p '[' >>. sepBy pJSON (p ',') .>> p ']'
    pJSON' :=
     choiceL [
       pObj    |>> (toMap >> Obj)
       pList   |>> List
       pString |>> String
       pNumber |>> Number
       pId >>= function
        | "true"  -> preturn t
        | "false" -> preturn f
        | "null"  -> preturn Nil
        | id      -> sprintf "Unexpected: %s" id |> fail
     ] "value"
    spaces >>. pJSON

  let pJSON = mkParser ()

  let read string =
    runParserOnString pJSON () "string" string
    |> function Success (result, (), _) -> result
              | Failure (error, _, ()) -> failwithf "%s" error

  open PPrint

  let pNil = txt "null"
  let pTrue = txt "true"
  let pFalse = txt "false"
  let colonLn = colon <^> line
  let commaLn = comma <^> line

  let inline isPrintable c = 32 <= int c && int c <= 255

  let pString s =
    let b = StringBuilder ()
    let inline appc (c: char) = b.Append c |> ignore
    let inline apps (s: string) = b.Append s |> ignore
    appc '"'
    String.iter
     <| function
         | '\"' -> apps "\\\""
         | '\\' -> apps "\\\\"
         | '\b' -> apps "\\b"
         | '\f' -> apps "\\f"
         | '\n' -> apps "\\n"
         | '\r' -> apps "\\r"
         | '\t' -> apps "\\t"
         | c ->
           if isPrintable c then
             appc c
           else
             let inline d x =
               let c = (x >>> 12) &&& 0xF
               (if c < 10 then int '0' else int 'a' - 10) |> (+) c |> char |> appc
               x <<< 4
             apps "\\u"
             int c |> d |> d |> d |> d |> ignore
     <| s
    appc '"'
    b.ToString () |> txt

  let rec pretty = function
    | Nil        -> pNil
    | Bool true  -> pTrue
    | Bool false -> pFalse
    | Number s   -> txt s
    | String s   -> pString s
    | List vs    ->
      vs |> Seq.map pretty |> punctuate commaLn |> hcat |> brackets |> gnest 1
    | Obj kvs ->
      kvs
      |> Seq.map (fun kv ->
         gnest 1 (pString kv.Key <^> colonLn <^> pretty kv.Value))
      |> punctuate commaLn |> hcat |> braces |> gnest 1

  let show v = pretty v |> PPrint.render None
