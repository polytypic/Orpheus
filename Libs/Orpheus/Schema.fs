// Copyright (C) by Vesa Karvonen

namespace Orpheus

open System
open PPrint

type Schema<'x> = | Schema
type Fields<'x> = | Fields

exception Constraint of Doc

module Schema =
  let explain (_: Schema<'x>) : Doc = failwith "XXX"
  let extract (_: Schema<'x>) (_: JSON) : 'x = failwith "XXX"

  let json: Schema<JSON> = Schema

  let (|?|) (_: Schema<'x>) (_: Doc) : Schema<'x> = Schema

  let (|>>) (_: Schema<'x>) (_: 'x -> 'y) : Schema<'y> = Schema

  let (.&.) (_: Schema<'x>) (_: Schema<'x>) : Schema<'x> = Schema
  let (.|.) (_: Schema<'x>) (_: Schema<'x>) : Schema<'x> = Schema

  let list (_: Schema<'x>) : Schema<list<'x>> = Schema

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
  let cmpL op t l rS = rS |>> fun r -> cmp op t l r r
  let cmpR op t lS r = lS |>> fun l -> cmp op t l r l

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
  let ( =.) l rS = rS |>> fun r -> eq l r
  let (.= ) lS r = lS |>> fun l -> eq r l

  let case s c = string .= s >>% c

  let lift (_: 'x) : Fields<'x> = Fields

  let (</>) (_: Fields<'x>) (_: Fields<unit>) : Fields<'x> = Fields
  let (<*>) (_: Fields<'x -> 'y>) (_: Fields<'x>) : Fields<'y> = Fields

  let req (_: string) (_: Schema<'x>) : Fields<'x> = Fields
  let opt (_: string) (_: Schema<'x>) : Fields<option<'x>> = Fields
  let def (_: string) (_: 'x) (_: Schema<'x>) : Fields<'x> = Fields

  let others: Fields<Map<string, JSON>> = Fields

  let obj (_: Fields<'x>) : Schema<'x> = Schema
