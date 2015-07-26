defmodule Mix.Tasks.AddAdmin do
  use Mix.Task
  alias ApathyDrive.Player

  def run([email]) do
    ApathyDrive.Repo.start_link
    case ApathyDrive.Repo.get_by(Player, email: email) do
      %Player{admin: true} ->
        Mix.shell.info "#{email} is already an admin"
      %Player{} = player ->
        player
        |> Map.put(:admin, true)
        |> ApathyDrive.Repo.update!

        Mix.shell.info "#{email} is now an admin"
      nil ->
        Mix.shell.info "#{email} does not exist"
    end
  end
end
