module UserProfile exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
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
    , username : String
    , loading : Bool
    }


type Msg
    = GotUser (Result Http.Error User)
    | UsernameInput String
    | Submit


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
    ( { user = Nothing, error = Nothing, username = "", loading = False }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsernameInput name ->
            ( { model | username = name }, Cmd.none )

        Submit ->
            if String.isEmpty model.username then
                ( { model | error = Just "Please enter a username." }, Cmd.none )

            else
                ( { model | error = Nothing, user = Nothing, loading = True }
                , Http.get
                    { url = "https://api.github.com/users/" ++ model.username
                    , expect = Http.expectJson GotUser userDecoder
                    }
                )

        GotUser (Ok user) ->
            ( { model | user = Just user, error = Nothing, loading = False }, Cmd.none )

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
            ( { model | error = Just errorMsg, loading = False }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ onSubmit Submit ]
            [ input
                [ type_ "text"
                , placeholder "GitHub username"
                , value model.username
                , onInput UsernameInput
                ]
                []
            , button [ type_ "submit" ] [ text "Search" ]
            ]
        , if model.loading then
            text "Loading..."

          else
            case ( model.username, model.user, model.error ) of
                ( "", _, _ ) ->
                    text ""

                ( _, Nothing, Just err ) ->
                    div [ class "error" ] [ text err ]

                ( _, Just user, _ ) ->
                    viewUser user

                _ ->
                    text ""
        ]


viewUser : User -> Html msg
viewUser user =
    div []
        [ h1 [] [ text (Maybe.withDefault user.login user.name) ]
        , p []
            [ a [ href user.html_url, target "_blank" ] [ strong [] [ text ("@" ++ user.login) ] ]
            ]
        , p [] [ text (Maybe.withDefault "No bio provided" user.bio) ]
        , p [] [ text (Maybe.withDefault "No location provided" user.location) ]
        , p []
            [ Maybe.map
                (\blogUrl ->
                    let
                        url =
                            if String.startsWith "http://" blogUrl || String.startsWith "https://" blogUrl then
                                blogUrl

                            else
                                "https://" ++ blogUrl
                    in
                    a [ href url, target "_blank" ] [ text blogUrl ]
                )
                user.blog
                |> Maybe.withDefault (text "No blog provided")
            ]
        , img
            [ class "avatar"
            , src (Maybe.withDefault "" user.avatar_url)
            , alt ("Photo of " ++ user.login)
            ]
            []
        ]
