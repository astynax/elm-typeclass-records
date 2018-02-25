module Classes.Monoid exposing (..)

import List
import Maybe
import Classes.Semigroup exposing (Semigroup, ImplementsSemigroup)


type alias Monoid a =
    { empty : a
    }


type alias ImplementsMonoid_ ctx a =
    { ctx
        | monoid : Monoid a
    }


type alias ImplementsMonoid a =
    ImplementsMonoid_ (ImplementsSemigroup a) a


list : Monoid (List a)
list =
    { empty = []
    }


maybe : ImplementsMonoid a -> Monoid (Maybe a)
maybe ctx =
    { empty = Just ctx.monoid.empty
    }


pair : ImplementsMonoid a -> Monoid ( a, a )
pair ctx =
    { empty =
        ( ctx.monoid.empty, ctx.monoid.empty )
    }


tuple : ImplementsMonoid a -> ImplementsMonoid b -> Monoid ( a, b )
tuple ctxa ctxb =
    { empty = ( ctxa.monoid.empty, ctxb.monoid.empty )
    }


concat : ImplementsMonoid a -> List a -> a
concat ctx =
    List.foldl ctx.semigroup.append ctx.monoid.empty


sum : Monoid number
sum =
    { empty = 0 }


product : Monoid number
product =
    { empty = 1 }


and : Monoid Bool
and =
    { empty = True }


or : Monoid Bool
or =
    { empty = False }
