defmodule Repo do
  use Ecto.Repo, adapter: Ecto.Adapters.Postgres

  def conf do
    parse_url "ecto://Adam:@localhost/apathy_drive"
  end

  def priv do
    app_dir(:apathy_drive, "priv/repo")
  end
end
