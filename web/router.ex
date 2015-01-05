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

  scope "/", ApathyDrive do
    pipe_through :browser # Use the default browser stack

    get "/", PageController, :index
    get  "/game/:id", PageController, :game, as: :game
    post "/game",     PageController, :game, as: :game_create
  end

  channel "mud", ApathyDrive.MUD
end

