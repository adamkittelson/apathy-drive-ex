defmodule Mix.Tasks.RemoveAdmin do
  use Mix.Task
  alias ApathyDrive.Player

  def run([email]) do
    ApathyDrive.Repo.start_link
    case ApathyDrive.Repo.get_by(Spirit, email: email) do
      %Player{admin: true} = player ->
        player
        |> Map.put(:admin, false)
        |> ApathyDrive.Repo.update!

        Mix.shell.info "#{email} is no longer an admin"
      %Player{} ->
        Mix.shell.info "#{email} is not an admin"
      nil ->
        Mix.shell.info "#{email} does not exist"
    end
  end
end
