defmodule Mix.Tasks.DropWorld do
  use Mix.Task

  def run(_) do
    Mix.Ecto.ensure_started(ApathyDrive.Repo, [])
    ApathyDrive.Repo.drop_world!
  end
end
