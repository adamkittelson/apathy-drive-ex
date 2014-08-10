defmodule ApathyDrive.Router do
  use Phoenix.Router
  use Phoenix.Router.Socket, mount: "/ws"

  plug Plug.Static, at: "/static", from: :apathy_drive
  get  "/",         ApathyDrive.PageController, :index, as: :page
  get  "/game/:id", ApathyDrive.PageController, :game, as: :game
  post "/game",     ApathyDrive.PageController, :game, as: :game_create

  channel "mud", ApathyDrive.MUD
end
