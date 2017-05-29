module Main exposing (main)

-- import Html.App    as App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import List

-- main : Program Never
main: Program Never Model Msg
main =
  Html.program
     { init          = init
     , update        = update
     , view          = view
     , subscriptions = subscriptions
     }

type alias Model =
  { chatMessage : List String
  , userMessage : String
  , userName : String
  }

type Msg
  = PostChatMessage
  | UpdateUserMessage String
  | NewChatMessage String
  | LoginMessage String

init : (Model, Cmd Msg)
init =
  ( Model [""] "" ""
  , Cmd.none
  )

type alias ChatMessage =
  { command: String
  , content: String
  }

view : Model -> Html Msg
view model =
  div []
    [ div [] []
    , input [ placeholder "message..."
            , autofocus True
            , value model.userMessage
            , onInput UpdateUserMessage
            ] []
    , button [ onClick PostChatMessage ] [ text "Submit" ]
    , div []  (List.map cMessage model.chatMessage)
  ]

wsUrl : String
wsUrl = "ws://localhost:3000"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PostChatMessage ->
      let
        message = model.userMessage
      in
        { model | userMessage = "" } ! [WebSocket.send "ws://localhost:3000/" message]

    UpdateUserMessage message ->
       { model | userMessage = message } ! []

    NewChatMessage message ->
       { model | chatMessage = message :: model.chatMessage } ! []

    LoginMessage message ->
       { model | userName = message } ! []


cMessage : String -> Html msg
cMessage message =
  div [] [text message]


subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen wsUrl NewChatMessage
