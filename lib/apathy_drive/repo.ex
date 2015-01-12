defmodule ApathyDrive.Repo do
  use Systems.Reload
  use Ecto.Repo, adapter: Ecto.Adapters.Postgres

  def conf do
    ApathyDrive.Config.get(:db)
    |> parse_url
  end

  def priv do
    app_dir(:apathy_drive, "priv/repo")
  end
end
