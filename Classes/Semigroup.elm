module Classes.Semigroup exposing (..)

import List
import Maybe


type alias Semigroup a =
    { append : a -> a -> a
    }


type alias ImplementsSemigroup a =
    { semigroup : Semigroup a
    }


list : Semigroup (List a)
list =
    { append = List.append }


maybe : ImplementsSemigroup a -> Semigroup (Maybe a)
maybe ctx =
    { append = Maybe.map2 ctx.semigroup.append }


pair : ImplementsSemigroup a -> Semigroup ( a, a )
pair ctx =
    { append =
        \( x1, y1 ) ( x2, y2 ) ->
            ( ctx.semigroup.append x1 x2
            , ctx.semigroup.append y1 y2
            )
    }


tuple : ImplementsSemigroup a -> ImplementsSemigroup b -> Semigroup ( a, b )
tuple ctxa ctxb =
    { append =
        \( x1, y1 ) ( x2, y2 ) ->
            ( ctxa.semigroup.append x1 x2
            , ctxb.semigroup.append y1 y2
            )
    }


sum : Semigroup number
sum =
    { append = (+) }


product : Semigroup number
product =
    { append = (*) }


and : Semigroup Bool
and =
    { append = (&&) }


or : Semigroup Bool
or =
    { append = (||) }
