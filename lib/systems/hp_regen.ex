defmodule Systems.HPRegen do
  use Systems.Reload
  def initialize do
    :timer.apply_interval(1_000, Systems.HPRegen, :regen_hp, [])
  end

  def regen_hp do
    Components.all(Components.HP) |> Enum.each(fn(entity) ->
      Components.HP.add(entity, regen_per_second(entity))
      if Entity.has_component?(entity, Components.Player) do
        Components.Player.send_message(entity, ["update prompt", "[HP=#{Components.HP.value(entity)}]:"])
      end
    end)
  end

  def regen_per_second(entity) do
    (Components.Level.value(entity) + 30) * Systems.Stat.modified(entity, "health") / 300
    |> Float.floor
  end
end
