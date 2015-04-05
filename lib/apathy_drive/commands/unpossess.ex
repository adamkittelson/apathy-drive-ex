defmodule Commands.Unpossess do
  use ApathyDrive.Command

  def keywords, do: ["unpossess"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You aren't possessing anything.</p>")
  end

  def execute(%Monster{spirit: spirit} = monster, _arguments) do
    send(spirit.pid, {:unpossess, monster})

    monster
    |> Map.put(:spirit, nil)
    |> Monster.set_abilities
  end

end
