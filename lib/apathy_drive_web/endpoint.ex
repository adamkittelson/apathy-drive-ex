defmodule ApathyDriveWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :apathy_drive

  socket "/ws", ApathyDriveWeb.UserSocket

  plug Plug.Static,
    at: "/", from: :apathy_drive, gzip: false,
    only: ~w(css fonts images js themes favicon.ico robots.txt)

  # Code reloading will only work if the :code_reloader key of
  # the :phoenix application is set to true in your config file.
  if code_reloading? do
    socket "/phoenix/live_reload/socket", Phoenix.LiveReloader.Socket
    plug Phoenix.LiveReloader
    plug Phoenix.CodeReloader
  end

  plug Plug.RequestId
  plug Plug.Logger

  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Poison

  plug Plug.MethodOverride
  plug Plug.Head

  plug Plug.Session,
    store: :cookie,
    key: "_apathy_drive_key",
    signing_salt: "y+gCAOZH"

  plug ApathyDriveWeb.Router
end
