module Classes.Functor exposing (..)

import Dict exposing (Dict)
import List
import Maybe


type alias Functor a b f g =
    { fmap : (a -> b) -> f -> g
    }


list : Functor a b (List a) (List b)
list =
    { fmap = List.map }


dict : Functor a b (Dict comparable a) (Dict comparable b)
dict =
    { fmap = Dict.map << always }


pair : Functor a b ( a, a ) ( b, b )
pair =
    { fmap = \f ( x, y ) -> ( f x, f y ) }


snd : Functor a b ( c, a ) ( c, b )
snd =
    { fmap = \f ( x, y ) -> ( x, f y ) }


maybe : Functor a b (Maybe a) (Maybe b)
maybe =
    { fmap = Maybe.map }
