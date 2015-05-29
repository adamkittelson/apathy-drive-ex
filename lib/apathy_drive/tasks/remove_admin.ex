defmodule Mix.Tasks.RemoveAdmin do
  use Mix.Task

  def run([name]) do
    ApathyDrive.Repo.start_link
    case ApathyDrive.Repo.get_by(Spirit, name: name) do
      %Spirit{admin: true} = spirit ->
        spirit
        |> Map.put(:admin, false)
        |> ApathyDrive.Repo.update

        Mix.shell.info "#{name} is no longer an admin"
      %Spirit{} ->
        Mix.shell.info "#{name} is not an admin"
      nil ->
        Mix.shell.info "#{name} does not exist"
    end
  end
end
