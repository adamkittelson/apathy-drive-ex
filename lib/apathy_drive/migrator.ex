defmodule ApathyDrive.Migrator do

  def start_link do
    Ecto.Migrator.run(ApathyDrive.Repo, "#{:code.priv_dir(:apathy_drive)}/repo/migrations", :up, all: true)

    Task.start_link(fn ->
      # this is the dirtiest hack of all time
      :noop
    end)
  end

end