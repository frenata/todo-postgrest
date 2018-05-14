module Todo exposing (..)

import Html exposing (Html, del, div, fieldset, form, input, label, table, td, text, tr)
import Html.Attributes exposing (autofocus, checked, class, id, name, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import PostgRest as PG exposing (Attribute, Request, Schema, Selection)
import Task
import Time


-- MODEL


type alias Model =
    { todos : List Todo -- current todos fetched from db
    , error : Maybe Http.Error -- last recorded error, for debugging
    , task : String -- the to-be-added task
    , shown : Shown -- tracks which todos to display
    }


type Shown
    = All
    | Completed
    | Uncompleted


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
    ( Model [] Nothing "" Uncompleted
    , fetchTodos
    )



-- UPDATE


{-| Enumeration of possible messages.
-}
type Msg
    = GetTodos (Result Http.Error (List Todo)) -- read from the db
    | PatchTodo (Result Http.Error Todo) -- modify the db
    | Toggle Todo
    | NewTask String
    | SendTask
    | Show Shown


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewTask t ->
            ( { model | task = t }, Cmd.none )

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
            , fetchTodos
            )

        Toggle todo ->
            ( model
            , Http.send PatchTodo <|
                PG.toHttpRequest server (toggleTodo todo)
            )

        Show s ->
            ( { model | shown = s }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ toggleView
        , viewTodos model.todos model.shown
        , addTodo model.task
        ]


{-| Toggle what tasks are shown: all / completed / uncompleted.
-}
toggleView : Html Msg
toggleView =
    let
        radio shown str def =
            label []
                [ input [ checked def, name "shown", type_ "radio", onClick (Show shown) ] []
                , text str
                ]
    in
    div []
        [ fieldset []
            [ radio All "All" False
            , radio Completed "Completed" False
            , radio Uncompleted "Uncompleted" True
            ]
        ]


{-| Render the existing todos into a table, click to toggle whether they are done.
-}
viewTodos : List Todo -> Shown -> Html Msg
viewTodos todos shown =
    let
        shouldDisplay x =
            case shown of
                All ->
                    True

                Completed ->
                    x.done

                Uncompleted ->
                    not x.done

        display x =
            tr
                [ class "todo"
                , onClick (Toggle x)
                ]
                [ td [ class "task" ]
                    [ if not x.done then
                        text x.task
                      else
                        del [] [ text x.task ]
                    ]
                ]
    in
    todos
        |> List.sortBy .id
        |> List.filter shouldDisplay
        |> List.map display
        |> table [ id "todos" ]


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


{-| Interestingly this schema is allowed to be a subset of the DB schema.
I'm not sure how to represent all PG datatypes within this library.
-}
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


{-| A selection of data to return from the database.
-}
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


{-| Create a new todo by specifying a task and accepting other defaults.
-}
newTodo : String -> Request Todo
newTodo task =
    PG.createOne todoSchema
        { change = PG.change .task task
        , select = todoSelection
        }


{-| Flip the 'done' status of tasks that match the id.
-}
toggleTodo : Todo -> Request Todo
toggleTodo todo =
    PG.updateOne todoSchema
        { change = PG.change .done (not todo.done)
        , where_ = PG.eq todo.id .id
        , select = todoSelection
        }


{-| Grab everything in the database and (on success) update the model.
-}
fetchTodos =
    Http.send GetTodos <|
        PG.toHttpRequest
            server
            (PG.readAll todoSchema todoSelection)
