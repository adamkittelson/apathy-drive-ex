defmodule ApathyDrive.Config.Dev do
  use ApathyDrive.Config

  config :router, port: 4000,
                  ssl: false,
                  # Full error reports are enabled
                  consider_all_requests_local: true

  config :plugs, code_reload: false

  config :logger, level: :debug
end


