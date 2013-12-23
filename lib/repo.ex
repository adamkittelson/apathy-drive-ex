defmodule ApathyDrive.Repo do
  use Ecto.Repo, adapter: Ecto.Adapters.Postgres

  def url do
    "ecto://Adam:@localhost/apathy_drive"
  end

  def priv do
    app_dir(:apathy_drive, "priv/repo")
  end
end
