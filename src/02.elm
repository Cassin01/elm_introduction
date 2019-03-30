-- Forms
-- https://guide.elm-lang.org/architecture/forms.html

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : String
  }


init : Model
init =
  Model "" "" "" ""



-- UPDATE


type Msg
  = Name String
  | Password String
  | PasswordAgain String
  | Age String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }
    Password password ->
      { model | password = password }
    PasswordAgain password ->
      { model | passwordAgain = password }
    Age age ->
      { model | age = age }



-- VIEW


view : Model -> Html Msg
view model =
  div [] [
    viewInput "text" "Name" model.name Name,
    viewInput "password" "Password" model.password Password,
    viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain,
    viewInput "text" "age" model.age Age,
    viewValidation model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
  if not (is_num model.age) then
    div [ style "color" "red" ] [ text  "Age Shoud be a number!" ]
  else if model.password /= model.passwordAgain then
    div [ style "color" "red" ] [ text "Passwords do not match!" ]
  else if String.length model.password <= 8 then
    div [ style "color" "red" ] [ text "Passwords should be longer than 8!" ]
  else if not (contain_num model.password) then
    div [ style "color" "red" ] [ text "Passwords should be contain number!" ]
  else
    div [] [
      div [ style "color" "green" ] [ text "OK" ],
      button [] [ text "Submit" ]
    ]

contain_num : String -> Bool
contain_num password = List.foldr (||) False
    (List.map (\z -> containx z password) (List.range 0 9))

is_num : String -> Bool
is_num age = List.foldr (&&) True
  (List.map (\x -> contain_num (String.fromChar x)) (String.toList age))

containx : Int -> String -> Bool
containx x password =
  String.contains (Debug.toString x) password