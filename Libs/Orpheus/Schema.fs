// Copyright (C) by Vesa Karvonen

namespace Orpheus

open System
open PPrint

type Schema<'x> = {
    SchemaDoc: Doc
  }
type Fields<'x> = {
    FieldDocs: list<Doc>
  }

exception Constraint of Doc

module Schema =
  let explain (xS: Schema<'x>) : Doc = xS.SchemaDoc
  let extract (_: Schema<'x>) (_: JSON) : 'x = failwith "XXX"

  let json: Schema<JSON> = {SchemaDoc = txt "json"}

  let (|?|) xS explain =
    {xS with SchemaDoc = explain}

  let (|>>) (xS: Schema<'x>) (_: 'x -> 'y) : Schema<'y> =
    {SchemaDoc = xS.SchemaDoc}

  let (.&.) (xS1: Schema<'x>) (xS2: Schema<'x>) : Schema<'x> =
    {SchemaDoc = xS1.SchemaDoc <+> txt "&" <.> xS2.SchemaDoc}
  let (.|.) (xS1: Schema<'x>) (xS2: Schema<'x>) : Schema<'x> =
    {SchemaDoc = xS1.SchemaDoc <+> txt "|" <.> xS2.SchemaDoc}

  let list (xS: Schema<'x>) : Schema<list<'x>> =
    {SchemaDoc = xS.SchemaDoc <^> comma <.> txt "..." |> brackets |> gnest 1}

  let mk label get =
    json
    |>> fun x ->
          match get x with
           | Some x -> x
           | None -> fmt "expected %s, but got %A" label x |> Constraint |> raise
    |?| txt label
  let string = mk "string" <| function String x -> Some x  | _ -> None
  let number = mk "number" <| function Number x -> Some x  | _ -> None
  let bool   = mk "bool"   <| function Bool   x -> Some x  | _ -> None
  let nil    = mk "null"   <| function Nil      -> Some () | _ -> None

  let num label using =
    number
    |>> fun x ->
          try using x
          with :? FormatException ->
               fmt "expected %s, but got %s" label x |> Constraint |> raise
             | :? OverflowException ->
               fmt "conversion of %s to %s overflowed" x label |> Constraint |> raise
    |?| txt label
  let int = num "int" int
  let float = num "float" float

  let (>>%) xS y = xS |>> fun _ -> y

  let cmp op t l r x =
    if op l r
    then x
    else fmt "expected %A %s %A" l t r |> Constraint |> raise
  let cmpL op t l rS =
    rS |>> fun r -> cmp op t l r r
       |?| (fmt "%A" l <+> txt t <+> rS.SchemaDoc)
  let cmpR op t lS r =
    lS |>> fun l -> cmp op t l r l
       |?| (lS.SchemaDoc <+> txt t <+> fmt "%A" r)

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

  let eq exp got =
    if exp = got
    then got
    else fmt "expected %A, but got %A" exp got |> Constraint |> raise
  let ( =.) l rS =
    rS |>> fun r -> eq l r
       |?| fmt "%A" l
  let (.= ) lS r =
    lS |>> fun l -> eq r l
       |?| fmt "%A" r

  let case s c = string .= s >>% c

  let lift (_: 'x) : Fields<'x> = {FieldDocs = []}

  let (</>) (xF: Fields<'x>) (uF: Fields<unit>) : Fields<'x> =
    {FieldDocs = uF.FieldDocs @ xF.FieldDocs}
  let (<*>) (x2yF: Fields<'x -> 'y>) (xF: Fields<'x>) : Fields<'y> =
    {FieldDocs = xF.FieldDocs @ x2yF.FieldDocs}

  let req (key: string) (xS: Schema<'x>) : Fields<'x> =
    {FieldDocs = [gnest 2 (fmt "%A" key <^> colon <+> xS.SchemaDoc)]}
  let opt (key: string) (xS: Schema<'x>) : Fields<option<'x>> =
    {FieldDocs = [gnest 1 (brackets (nest 2 (fmt "%A" key <^> colon <+> xS.SchemaDoc)))]}
  let def (key: string) (x: 'x) (xS: Schema<'x>) : Fields<'x> =
    {FieldDocs = [gnest 1 (brackets (nest 2 (fmt "%A" key <^> colon <+> xS.SchemaDoc <+> equals <+> fmt "%A" x)))]}

  let others: Fields<Map<string, JSON>> =
    {FieldDocs = [txt "..."]}

  let obj (xF: Fields<'x>) : Schema<'x> =
    {SchemaDoc =
       xF.FieldDocs
       |> List.rev
       |> punctuate comma
       |> vsep
       |> fun d -> linebreak <^> d
       |> nest 2
       |> fun d -> d <^> linebreak
       |> group
       |> braces}
