defmodule ApathyDrive.Endpoint do
  use Phoenix.Endpoint, otp_app: :apathy_drive

  plug Plug.Static,
    at: "/", from: :apathy_drive,
    only: ~w(css images js favicon.ico robots.txt)

  plug Plug.Logger

  # Code reloading will only work if the :code_reloader key of
  # the :phoenix application is set to true in your config file.
  if code_reloading? do
    plug Phoenix.LiveReloader
    plug Phoenix.CodeReloader
  end

  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Poison

  plug Plug.MethodOverride
  plug Plug.Head

  plug Plug.Session,
    store: :cookie,
    key: "_apathy_drive_key",
    signing_salt: "y+gCAOZH",
    encryption_salt: "mMJ4NoI3"

  plug :router, ApathyDrive.Router
end
