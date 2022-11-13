module Pages.Home_ exposing (Model, Msg, page)

import Css
import Gen.Params.Home_ exposing (Params)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Page
import Request
import Shared
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
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



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
viewBody _ =
    [ displayCue "let there be light" ]


displayCue : String -> Html Msg
displayCue secret =
    secret
        |> String.toList
        |> List.map (displayCharacter >> spanify)
        |> Html.div
            [ Attributes.css
                [ Css.displayFlex
                , Css.fontFamily Css.monospace
                , Css.fontSize (Css.em 1.75)
                ]
            ]


displayCharacter : Char -> String
displayCharacter char =
    if char == ' ' then
        " "

    else
        "_"


spanify : String -> Html Msg
spanify string =
    Html.span
        [ Attributes.css
            [ Css.margin2 Css.zero (Css.em 0.16)
            , Css.minWidth (Css.em 0.16)
            ]
        ]
        [ Html.text string ]
