// Copyright (C) by Vesa Karvonen

namespace Orpheus

open PPrint

type Schema<'x>
type Fields<'x>

exception Constraint of Doc

module Schema =
  val explain: Schema<'x> -> Doc
  val extract: Schema<'x> -> JSON -> 'x

  val json: Schema<JSON>

  val list: Schema<'x> -> Schema<list<'x>>
  val string: Schema<string>
  val number: Schema<string>
  val bool: Schema<bool>
  val nil: Schema<unit>

  val float: Schema<float>
  val int: Schema<int>

  val case: string -> 'x -> Schema<'x>

  val (|?|): Schema<'x> -> Doc -> Schema<'x>

  val (.&.): Schema<'x> -> Schema<'x> -> Schema<'x>
  val (.|.): Schema<'x> -> Schema<'x> -> Schema<'x>

  val (|>>): Schema<'x> -> ('x -> 'y) -> Schema<'y>

  val  ( <.):        'x  -> Schema<'x> -> Schema<'x> when 'x: comparison
  val  (.< ): Schema<'x> ->        'x  -> Schema<'x> when 'x: comparison
  val ( <=.):        'x  -> Schema<'x> -> Schema<'x> when 'x: comparison
  val (.<= ): Schema<'x> -> 'x         -> Schema<'x> when 'x: comparison
  val  ( >.):        'x  -> Schema<'x> -> Schema<'x> when 'x: comparison
  val  (.> ): Schema<'x> ->        'x  -> Schema<'x> when 'x: comparison
  val ( >=.):        'x  -> Schema<'x> -> Schema<'x> when 'x: comparison
  val (.>= ): Schema<'x> ->        'x  -> Schema<'x> when 'x: comparison

  val ( <>.):        'x  -> Schema<'x> -> Schema<'x> when 'x: equality
  val (.<> ): Schema<'x> ->        'x  -> Schema<'x> when 'x: equality
  val  ( =.):        'x  -> Schema<'x> -> Schema<'x> when 'x: equality
  val  (.= ): Schema<'x> ->        'x  -> Schema<'x> when 'x: equality

  val lift: 'x -> Fields<'x>

  val (<*>): Fields<'x -> 'y> -> Fields<'x> -> Fields<'y>
  val (</>): Fields<'x> -> Fields<unit> -> Fields<'x>

  val req: string ->       Schema<'x> -> Fields<       'x >
  val opt: string ->       Schema<'x> -> Fields<option<'x>>
  val def: string -> 'x -> Schema<'x> -> Fields<       'x >

  val others: Fields<Map<string, JSON>>

  val obj: Fields<'x> -> Schema<'x>
