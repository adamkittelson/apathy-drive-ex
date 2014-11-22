defmodule ApathyDrive.Router do
  use Phoenix.Router
  use Phoenix.Router.Socket, mount: "/ws"

  pipeline :browser do
    plug :accepts, ~w(html)
    plug :fetch_session
  end

  pipeline :api do
    plug :accepts, ~w(json)
  end

  scope "/" do
    pipe_through :browser # Use the default browser stack

    get "/", ApathyDrive.PageController, :index
    get  "/game/:id", ApathyDrive.PageController, :game, as: :game
    post "/game",     ApathyDrive.PageController, :game, as: :game_create
  end

  channel "mud", ApathyDrive.MUD
end

