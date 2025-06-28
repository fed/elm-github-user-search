module UserProfile exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode


type alias User =
    { login : String
    , name : Maybe String
    , bio : Maybe String
    , location : Maybe String
    , blog : Maybe String
    , avatar_url : Maybe String
    , html_url : String
    }


type alias Model =
    { user : Maybe User
    , error : Maybe String
    }


type Msg
    = GotUser (Result Http.Error User)


userDecoder : Decode.Decoder User
userDecoder =
    Decode.map7 User
        (Decode.field "login" Decode.string)
        (Decode.field "name" (Decode.nullable Decode.string))
        (Decode.field "bio" (Decode.nullable Decode.string))
        (Decode.field "location" (Decode.nullable Decode.string))
        (Decode.field "blog" (Decode.nullable Decode.string))
        (Decode.field "avatar_url" (Decode.nullable Decode.string))
        (Decode.field "html_url" Decode.string)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { user = Nothing, error = Nothing }
    , Http.get
        { url = "https://api.github.com/users/fed"
        , expect = Http.expectJson GotUser userDecoder
        }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotUser (Ok user) ->
            ( { model | user = Just user, error = Nothing }, Cmd.none )

        GotUser (Err httpError) ->
            let
                errorMsg =
                    case httpError of
                        Http.BadUrl url ->
                            "Bad URL: " ++ url

                        Http.Timeout ->
                            "Request timed out."

                        Http.NetworkError ->
                            "Network error."

                        Http.BadStatus statusCode ->
                            "HTTP error " ++ String.fromInt statusCode

                        Http.BadBody message ->
                            "Bad body: " ++ message
            in
            ( { model | error = Just errorMsg }, Cmd.none )


view : Model -> Html Msg
view model =
    case ( model.user, model.error ) of
        ( Nothing, Nothing ) ->
            text "Loading..."

        ( Nothing, Just err ) ->
            div [ class "error" ] [ text err ]

        ( Just user, _ ) ->
            div []
                [ h1 [] [ text (Maybe.withDefault user.login user.name) ]
                , p []
                    [ a [ href user.html_url, target "_blank" ] [ strong [] [ text ("@" ++ user.login) ] ]
                    ]
                , p [] [ text (Maybe.withDefault "No bio provided" user.bio) ]
                , p [] [ text (Maybe.withDefault "No location provided" user.location) ]
                , p []
                    [ case user.blog of
                        Just blogUrl ->
                            a [ href ("https://" ++ blogUrl), target "_blank" ] [ text blogUrl ]

                        Nothing ->
                            text "No blog provided"
                    ]
                , img
                    [ class "avatar"
                    , src (Maybe.withDefault "" user.avatar_url)
                    , alt ("Photo of " ++ user.login)
                    ]
                    []
                ]
