module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html)
import Classes.Functor as Functor
import Classes.Foldable as Foldable
import Classes.Monoid as Monoid
import Classes.Semigroup as Semigroup


main : Html ()
main =
    let
        fmap =
            .fmap Functor.dict
                << .fmap Functor.snd
                << .fmap Functor.list
                << .fmap Functor.maybe

        fold =
            let
                ctx =
                    { semigroup = Semigroup.sum
                    , monoid = Monoid.sum
                    }
            in
                .foldMap Foldable.dict ctx
                    << .foldMap Foldable.snd ctx
                    << .foldMap Foldable.list ctx
                    << .foldMap Foldable.maybe ctx
                    <| Foldable.length Foldable.list
    in
        Dict.fromList
            [ ( "a", ( "foo", [ Just [] ] ) )
            , ( "b", ( "bar", [ Just [1], Nothing, Just [2, 3] ] ) )
            ]
            |> fmap ((::) 0) -- prepend 0 to all deep lists
            |> fold          -- returns 6
            |> toString
            |> Html.text
