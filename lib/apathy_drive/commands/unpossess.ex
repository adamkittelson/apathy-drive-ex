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
    |> Map.put(:max_hp,   monster.max_hp   - (10 * spirit.level))
    |> Map.put(:hp_regen, monster.hp_regen - spirit.level)
    |> Monster.set_abilities
    |> Monster.save
  end

end
