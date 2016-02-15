defmodule ApathyDrive.Migrator do

  def start_link do
    Task.start_link(fn ->
      Ecto.Migrator.run(ApathyDrive.Repo, "#{:code.priv_dir(:apathy_drive)}/repo/migrations", :up, all: true)
    end)
  end

end