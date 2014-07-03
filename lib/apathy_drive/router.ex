defmodule ApathyDrive.Router do
  use Phoenix.Router
  use Phoenix.Router.Socket, mount: "/ws"

  plug Plug.Static, at: "/static", from: :apathy_drive
  get  "/",         ApathyDrive.Controllers.Pages, :index, as: :page
  get  "/game/:id", ApathyDrive.Controllers.Pages, :game, as: :game
  post "/game",     ApathyDrive.Controllers.Pages, :game, as: :game_create

  channel "mud", ApathyDrive.MUD
end
