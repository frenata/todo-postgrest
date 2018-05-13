module Todo exposing (..)

import Html exposing (Html, div, form, input, table, td, text, tr)
import Html.Attributes exposing (autofocus, class, id, placeholder, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import PostgRest as PG
    exposing
        ( Attribute
        , Request
        , Schema
        , Selection
        )
import Task
import Time


-- MODEL


type alias Model =
    { todos : List Todo
    , error : Maybe Http.Error
    , task : String
    }


type alias Todo =
    { task : String
    , done : Bool
    , id : Int
    }


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init =
    ( Model [] Nothing ""
    , Http.send GetTodos <|
        PG.toHttpRequest
            server
            getTodos
    )



-- UPDATE


{-| Enumeration of possible messages.
-}
type Msg
    = GetTodos (Result Http.Error (List Todo))
    | PatchTodo (Result Http.Error Todo)
    | Toggle Todo
    | NewTask String
    | SendTask
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewTask t ->
            ( { model | task = t }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        SendTask ->
            ( { model | task = "" }
            , Http.send PatchTodo <|
                PG.toHttpRequest server (newTodo model.task)
            )

        GetTodos (Err e) ->
            ( { model | error = Just e }, Cmd.none )

        GetTodos (Ok newTodos) ->
            ( { model | todos = newTodos }, Cmd.none )

        PatchTodo (Err e) ->
            ( { model | error = Just e }, Cmd.none )

        PatchTodo (Ok newTodos) ->
            ( model
            , Http.send GetTodos <|
                PG.toHttpRequest
                    server
                    getTodos
            )

        Toggle todo ->
            ( model
            , Http.send PatchTodo <|
                PG.toHttpRequest server (toggleTodo todo)
            )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewTodos model.todos
        , addTodo model.task
        ]


{-| Render the existing todos into a table, click to toggle whether they are done.
-}
viewTodos : List Todo -> Html Msg
viewTodos todos =
    table [ id "todos" ] <|
        List.map
            (\x ->
                tr
                    [ class "todo"
                    , onClick (Toggle x)
                    ]
                    [ td [ class "id" ] [ text <| toString x.id ]
                    , td [ class "task" ] [ text x.task ]
                    , td [ class "done" ] [ text <| toString x.done ]
                    ]
            )
            (List.sortBy .id todos)


{-| Form to add a todo.
-}
addTodo : String -> Html Msg
addTodo task =
    form [ id "new-todo", onSubmit SendTask ]
        [ input [ onInput NewTask, placeholder "What Do?", autofocus True, value task ] [] ]



-- SUBS


{-| Handle outside events.
-}
subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none



-- DATABASE


todoSchema :
    Schema x
        { id : Attribute Int
        , done : Attribute Bool
        , task : Attribute String
        }
todoSchema =
    PG.schema "todos"
        { id = PG.int "id"
        , done = PG.bool "done"
        , task = PG.string "task"
        }


todoSelection :
    Selection
        { attributes
            | task : Attribute String
            , done : Attribute Bool
            , id : Attribute Int
        }
        Todo
todoSelection =
    PG.map3 Todo
        (PG.field .task)
        (PG.field .done)
        (PG.field .id)


server =
    { timeout = Just Time.second
    , token = Nothing
    , url = "http://localhost:3000"
    }


newTodo : String -> Request Todo
newTodo task =
    PG.createOne todoSchema
        { change = PG.change .task task
        , select = todoSelection
        }


toggleTodo : Todo -> Request Todo
toggleTodo todo =
    PG.updateOne todoSchema
        { change = PG.change .done (not todo.done)
        , where_ = PG.eq todo.id .id
        , select = todoSelection
        }


getTodos : Request (List Todo)
getTodos =
    PG.readAll todoSchema todoSelection
