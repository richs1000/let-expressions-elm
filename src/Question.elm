module Question exposing (..)

import RandomStuff exposing (pickOne, pickABunch, compressList)


type QuestionFormat
    = FillInTheBlank
    | MultipleChoice


type alias Question =
    { question : List String
    , distractors : List ResponseAndFeedback
    , answer : ResponseAndFeedback
    , format : QuestionFormat
    }


type alias ResponseAndFeedback =
    ( String, String )


emptyQuestion : Question
emptyQuestion =
    { question = []
    , distractors = []
    , answer = ( "", "" )
    , format = FillInTheBlank
    }


silly1 : Int -> Int -> Int -> Int -> Int -> Int -> Int
silly1 z b c d f h =
    let
        x =
            if z > b then
                z
            else
                c

        y =
            x + z + d
    in
        if x > h then
            x * f
        else
            y * y


newQuestion : List Int -> Int -> Question
newQuestion randomValues index =
    let
        b =
            pickOne randomValues [0..4] 1

        c =
            pickOne (List.drop 1 randomValues) [0..4] 1

        d =
            pickOne (List.drop 2 randomValues) [0..4] 1

        f =
            pickOne (List.drop 3 randomValues) [0..4] 1

        g =
            pickOne (List.drop 4 randomValues) [1..4] 1

        h =
            pickOne (List.drop 5 randomValues) [2..4] 1

        question' =
            [ "What is the value of ans after the following "
            , "ML expressions are evaluated?"
            , ""
            , "fun silly1 (z : int) ="
            , "    let"
            , "        val x = if z > " ++ toString b ++ " then z else " ++ toString c
            , "        val y = x + z + " ++ toString d
            , "    in"
            , "        if x > " ++ toString h ++ " then x * " ++ toString f ++ " else y * y"
            , ""
            , "val ans = silly1(" ++ toString g ++ ")"
            , ""
            ]

        answer' =
            toString (silly1 g b c d f h)

        distractors =
            [ toString g
            , toString (silly1 h b c d f g)
            , toString (silly1 c h b d f g)
            , toString (silly1 f d h c b g)
            , toString (silly1 d c b h f g)
            , toString (silly1 g c d f h b)
            , toString (silly1 f d c g b h)
            , toString (silly1 b g d c h f)
            , toString (silly1 b g d h g c)
            , toString (silly1 b f h c g d)
            ]

        ( _, distractors' ) =
            List.partition (\d -> d == answer') (compressList distractors)
    in
        { question = question'
        , distractors = List.map (\dis -> ( dis, "Incorrect." )) distractors'
        , answer = ( answer', "Correct" )
        , format = MultipleChoice
        }


findFeedback : String -> String -> List ResponseAndFeedback -> String
findFeedback answer response distractors =
    case distractors of
        [] ->
            "Incorrect. The answer is " ++ answer

        d :: ds ->
            if ((fst d) == response || ((fst d) == "")) then
                (snd d) ++ " The answer is " ++ answer
            else
                findFeedback answer response ds
