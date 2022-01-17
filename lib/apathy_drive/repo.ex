defmodule ApathyDrive.Repo do
  use Ecto.Repo,
    otp_app: :apathy_drive,
    adapter: Ecto.Adapters.Postgres

  use Scrivener, page_size: 10

  def save!(%{:__struct__ => _module} = _current_struct) do
    raise "save! is deprecated"
  end
end
