defmodule Repo do
  use Systems.Reload
  use Ecto.Repo, adapter: Ecto.Adapters.Postgres

  def conf do
    Phoenix.Config.get([:mud, :db])
    |> parse_url
  end

  def priv do
    app_dir(:apathy_drive, "priv/repo")
  end
end
