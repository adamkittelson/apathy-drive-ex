defmodule ApathyDrive.Router do
  use Phoenix.Router

  pipeline :browser do
    plug :accepts, ~w(html)
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
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

  socket "/ws", ApathyDrive do
    channel "mud", MUD
  end
end

