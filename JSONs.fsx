#I __SOURCE_DIRECTORY__ ;;

#r "Libs\\JSONs\\bin\\Release\\JSONs.dll" ;;

open System ;;
open JSONs.Spec ;;

module Example =
  let gt (min: float) x = if min < x then x else failwith "gt"
  let none m = if Map.isEmpty m then () else failwith "none"

  let Int = Float |>> int

  type Dimensions = {
      length: float
      width: float
      height: float
    }

  type Product = {
      id: int
      name: string
      price: float
      tags: list<string>
      dimensions: option<Dimensions>
    }

  let Dimensions =
    Obj (con (fun length width height ->
                {length = length; width = width; height = height})
         <*> req "length" Float /> gt 0.0
         <*> req "width"  Float /> gt 0.0
         <*> req "height" Float /> gt 0.0
         </> others /> none)

  let Product =
    Obj (con (fun id name price tags dimensions ->
                {id = id; name = name; price = price; tags = tags; dimensions = dimensions})
         <*> req "id" Int
         <*> req "name" String
         <*> req "price" Float /> gt 0.0
         <*> def "tags" (List String) []
         <*> opt "dimensions" Dimensions
         </> others /> none)

  let ProductSet = List Product
