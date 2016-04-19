defmodule ApathyDrive.Migrator do
  alias ApathyDrive.{Repo, Mobile, RoomUnity}

  def start_link do
    Ecto.Migrator.run(Repo, "#{:code.priv_dir(:apathy_drive)}/repo/migrations", :up, all: true)

    if System.get_env("RESET_GAME") do
      Repo.delete_all(Mobile)
      Repo.delete_all(RoomUnity)
    end

    Task.start_link(fn ->
      :noop
    end)
  end

end