defmodule ApathyDrive.Migrator do
  alias ApathyDrive.{Repo, Monster}

  def start_link do
    Ecto.Migrator.run(Repo, "#{:code.priv_dir(:apathy_drive)}/repo/migrations", :up, all: true)

    if System.get_env("RESET_GAME") do
      Repo.delete_all(Monster)
      Repo.update_all(Spirit, set: [name: nil, class_id: nil, level: 1, experience: 0, inventory: [], equipment: []])
    end

    Task.start_link(fn ->
      :noop
    end)
  end

end