module Pages.Home_ exposing (Model, Msg, page)

import Gen.Params.Home_ exposing (Params)
import Html exposing (Html)
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
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody _ =
    [ displayCue "let there be light" ]


displayCue : String -> Html Msg
displayCue secret =
    secret
        |> String.toList
        |> List.map (displayCharacter >> spanify)
        |> Html.div []


displayCharacter : Char -> String
displayCharacter char =
    if char == ' ' then
        " "

    else
        "_"


spanify : String -> Html Msg
spanify string =
    Html.span [] [ Html.text string ]
