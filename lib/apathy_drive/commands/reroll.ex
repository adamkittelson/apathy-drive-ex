defmodule Commands.Reroll do
  use ApathyDrive.Command

  def keywords, do: ["reroll"]

  def execute(%Spirit{} = spirit, _arguments) do
    send(spirit.socket_pid, :go_home)

    spirit
    |> Map.put(:name, nil)
    |> Map.put(:faction, nil)
    |> Map.put(:alignment, nil)
    |> Spirit.save
  end

  def execute(%Monster{spirit: _spirit} = monster, _arguments) do
    monster
    |> Monster.send_scroll("<p>You must unpossess #{monster.name} to reroll.</p>")
  end

end
