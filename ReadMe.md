# ToDo PostgREST

This is a simple proof of concept, demonstrating how to write an Elm SPA that is backed by a [PostgREST](http://postgrest.org/) server for persistence.

## Requirements

 * Docker
 * Elm
 * [elm-install](https://github.com/gdotdesign/elm-github-install), a non-standard tool used to install the development branch of `elm-postgrest`.

## Development

 * `elm-install`
   to download all the dependencies as specified in `elm-package.json`
 * `docker-compose up`
   to run the postgres/postgREST servers
 * `elm-reactor`
   to run an interactive browser session that will render the Elm app

## Usage

Navigate to the Elm app running on (probably) [localhost](http://localhost:8000/Todo.elm), and start adding some todos, then toggling their finished status.

## ToDo

 * security (via JWTs?)
 * support for showing all/only-completed/only-uncompleted todos
 * support for permanently deleting todos
