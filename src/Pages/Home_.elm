module Pages.Home_ exposing (Model, Msg, page)

import Css
import Gen.Params.Home_ exposing (Params)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Page
import Request
import Set exposing (Set)
import Shared
import Task
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page _ _ =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { secret : Maybe String
    , secretSet : Set String
    , guesses : Set String
    }


init : ( Model, Cmd Msg )
init =
    ( { secret = Nothing
      , secretSet = Set.empty
      , guesses = Set.empty
      }
    , Task.succeed (InitializeSecret "this is a secret") |> Task.perform identity
    )



-- UPDATE


type Msg
    = InitializeSecret String
    | RecordGuess String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitializeSecret secret ->
            ( { model
                | secret = Just secret
                , secretSet = Set.fromList (String.split "" secret)
              }
            , Cmd.none
            )

        RecordGuess character ->
            ( { model
                | guesses = Set.insert character model.guesses
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Home"
    , body = viewBody model |> List.map Html.toUnstyled
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ displayCue model, displayButtons model ]


displayCue : Model -> Html Msg
displayCue model =
    case model.secret of
        Nothing ->
            Html.text ""

        Just secret ->
            secret
                |> String.split ""
                |> List.map (displayCharacter model >> spanify)
                |> Html.div
                    [ Attributes.css
                        [ Css.displayFlex
                        , Css.marginBottom (Css.px 16)
                        , Css.fontFamily Css.monospace
                        , Css.fontSize (Css.px 20)
                        ]
                    ]


displayCharacter : Model -> String -> String
displayCharacter model string =
    if string == " " then
        " "

    else if Set.member string model.guesses then
        string

    else
        "_"


spanify : String -> Html Msg
spanify string =
    Html.span
        [ Attributes.css
            [ Css.margin2 Css.zero (Css.px 4)
            , Css.minWidth (Css.px 4)
            ]
        ]
        [ Html.text string ]


buttonSize : Float
buttonSize =
    40


buttonMargin : Float
buttonMargin =
    4


displayButtons : Model -> Html Msg
displayButtons model =
    let
        buttonsPerRow =
            5
    in
    "abcdefghijklmnopqrstuvwxyz"
        |> String.split ""
        |> List.map (buttonify model)
        |> Html.div
            [ Attributes.css
                [ Css.displayFlex
                , Css.flexWrap Css.wrap
                , Css.width <|
                    Css.px
                        (buttonsPerRow
                            * (buttonSize + (2 * buttonMargin))
                        )
                , Css.fontFamily Css.monospace
                , Css.fontSize (Css.px 20)
                ]
            ]


buttonify : Model -> String -> Html Msg
buttonify model string =
    Html.button
        (Attributes.css
            [ Css.width (Css.px buttonSize)
            , Css.height (Css.px buttonSize)
            , Css.margin (Css.px buttonMargin)
            , Css.padding (Css.px 8)
            , Css.borderRadius (Css.px 4)
            , Css.borderWidth Css.zero
            ]
            :: (if Set.member string model.guesses then
                    if Set.member string model.secretSet then
                        [ Attributes.css
                            [ Css.backgroundColor (Css.rgb 124 185 232)
                            , Css.borderColor Css.transparent
                            ]
                        ]

                    else
                        [ Attributes.disabled True ]

                else
                    [ Attributes.css [ Css.cursor Css.pointer ]
                    , Events.onClick <| RecordGuess string
                    ]
               )
        )
        [ Html.text string ]
