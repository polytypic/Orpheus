// Copyright (C) by Vesa Karvonen

namespace Orpheus

open System
open PPrint

type Schema<'x> = {
    Doc: Doc
  }
type Fields<'x> = {
    Docs: list<Doc>
  }

exception Constraint of Doc

module Schema =
  let explain (xS: Schema<'x>) : Doc = xS.Doc
  let extract (_: Schema<'x>) (_: JSON) : 'x = failwith "XXX"

  let json: Schema<JSON> = {Doc = txt "json"}

  let (|?|) xS explain =
    {xS with Doc = explain}

  let (|>>) (xS: Schema<'x>) (_: 'x -> 'y) : Schema<'y> =
    {Doc = xS.Doc}

  let (.&.) (xS1: Schema<'x>) (xS2: Schema<'x>) : Schema<'x> =
    {Doc = xS1.Doc <+> txt "&" <.> xS2.Doc}
  let (.|.) (xS1: Schema<'x>) (xS2: Schema<'x>) : Schema<'x> =
    {Doc = xS1.Doc <+> txt "|" <.> xS2.Doc}

  let list (xS: Schema<'x>) : Schema<list<'x>> =
    {Doc = xS.Doc <^> comma <.> txt "..." |> brackets |> gnest 1}

  let mk label get =
    json |?| txt label |>> fun x ->
    match get x with
     | Some x -> x
     | None -> fmt "expected %s, but got %A" label x |> Constraint |> raise
  let string = mk "string" <| function String x -> Some x  | _ -> None
  let number = mk "number" <| function Number x -> Some x  | _ -> None
  let bool   = mk "bool"   <| function Bool   x -> Some x  | _ -> None
  let nil    = mk "null"   <| function Nil      -> Some () | _ -> None

  let num label using =
    number |?| txt label |>> fun x ->
    try using x
    with :? FormatException ->
         fmt "expected %s, but got %s" label x |> Constraint |> raise
       | :? OverflowException ->
         fmt "conversion of %s to %s overflowed" x label |> Constraint |> raise
  let int = num "int" int
  let float = num "float" float

  let (>>%) xS y = xS |>> fun _ -> y

  let cmp op t l r x =
    if op l r then x else fmt "expected %A %s %A" l t r |> Constraint |> raise
  let cmpL op t l rS =
    rS |?| (fmt "%A" l <+> txt t <+> rS.Doc) |>> fun r -> cmp op t l r r
  let cmpR op t lS r =
    lS |?| (lS.Doc <+> txt t <+> fmt "%A" r) |>> fun l -> cmp op t l r l

  let ( <.) l rS = cmpL (<) "<" l rS
  let (.< ) lS r = cmpR (<) "<" lS r
  let ( >.) l rS = cmpL (>) ">" l rS
  let (.> ) lS r = cmpR (>) ">" lS r

  let ( <=.) l rS = cmpL (<=) "<=" l rS
  let (.<= ) lS r = cmpR (<=) "<=" lS r
  let ( >=.) l rS = cmpL (>=) ">=" l rS
  let (.>= ) lS r = cmpR (>=) ">=" lS r

  let ( <>.) l rS = cmpL (<>) "<>" l rS
  let (.<> ) lS r = cmpR (<>) "<>" lS r

  let eq e g =
    if e = g then g else fmt "expected %A, but got %A" e g |> Constraint |> raise
  let ( =.) l rS = rS |?| fmt "%A" l |>> fun r -> eq l r
  let (.= ) lS r = lS |?| fmt "%A" r |>> fun l -> eq r l

  let case s c = string .= s >>% c

  let lift (_: 'x) : Fields<'x> = {Docs = []}

  let (</>) (xF: Fields<'x>) (uF: Fields<unit>) : Fields<'x> =
    {Docs = uF.Docs @ xF.Docs}
  let (<*>) (x2yF: Fields<'x -> 'y>) (xF: Fields<'x>) : Fields<'y> =
    {Docs = xF.Docs @ x2yF.Docs}

  let req (key: string) (xS: Schema<'x>) : Fields<'x> =
    {Docs = [gnest 2 (fmt "%A" key <^> colon <+> xS.Doc)]}
  let opt (key: string) (xS: Schema<'x>) : Fields<option<'x>> =
    {Docs = [gnest 1 (brackets (nest 2 (fmt "%A" key <^> colon <+> xS.Doc)))]}
  let def (key: string) (x: 'x) (xS: Schema<'x>) : Fields<'x> =
    {Docs = [gnest 1 (brackets (nest 2 (fmt "%A" key <^> colon <+> xS.Doc <+> equals <+> fmt "%A" x)))]}

  let others: Fields<Map<string, JSON>> =
    {Docs = [txt "..."]}

  let obj (xF: Fields<'x>) : Schema<'x> =
    {Doc =
       xF.Docs
       |> List.rev
       |> punctuate comma
       |> vsep
       |> fun d -> linebreak <^> d
       |> nest 2
       |> fun d -> d <^> linebreak
       |> group
       |> braces}
