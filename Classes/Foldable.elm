module Classes.Foldable exposing (..)

import Dict exposing (Dict)
import List
import Maybe
import Classes.Monoid as Monoid exposing (Monoid, ImplementsMonoid)
import Classes.Semigroup as Semigroup


type alias Foldable a b f =
    { foldMap :
        ImplementsMonoid b
        -> (a -> b)
        -> f
        -> b
    }


type alias ImplementsFoldable_ ctx a b f =
    { ctx
        | foldable : Foldable a b f
    }


type alias ImplementsFoldable a b f =
    ImplementsFoldable_ (ImplementsMonoid b) a b f


list : Foldable a b (List a)
list =
    { foldMap = \ctx f l -> Monoid.concat ctx <| List.map f l
    }


maybe : Foldable a b (Maybe a)
maybe =
    { foldMap =
        \ctx f m ->
            case Maybe.map f m of
                Just x ->
                    x

                Nothing ->
                    ctx.monoid.empty
    }


pair : Foldable a b ( a, a )
pair =
    { foldMap =
        \ctx f ( x, y ) ->
            ctx.semigroup.append (f x) (f y)
    }


snd : Foldable a b ( c, a )
snd =
    { foldMap =
        \_ f ( _, y ) -> f y
    }


dict : Foldable a b (Dict comparable a)
dict =
    { foldMap =
        \ctx f d -> list.foldMap ctx f <| Dict.values d
    }


length : Foldable a number f -> f -> number
length f =
    f.foldMap
        { semigroup = Semigroup.sum
        , monoid = Monoid.sum
        }
        (always 1)


sum : Foldable number number f -> f -> number
sum f =
    f.foldMap
        { semigroup = Semigroup.sum
        , monoid = Monoid.sum
        }
        identity


product : Foldable number number f -> f -> number
product f =
    f.foldMap
        { semigroup = Semigroup.product
        , monoid = Monoid.product
        }
        identity


toList : Foldable a (List a) f -> f -> List a
toList f =
    f.foldMap
        { semigroup = Semigroup.list
        , monoid = Monoid.list
        }
        (flip (::) [])
