defmodule Systems.Regen do
  use Systems.Reload

  def initialize do
    :timer.apply_interval(1_000, Systems.Regen, :regen, [])
  end

  def regen do
    regen_hp
    regen_mana
    update_prompt
  end

  def regen_hp do
    Components.all(Components.HP) |> Enum.each(fn(entity) ->
      Components.HP.add(entity, hp_regen_per_second(entity))
    end)
  end

  def regen_mana do
    Components.all(Components.Mana) |> Enum.each(fn(entity) ->
      Components.Mana.add(entity, mana_regen_per_second(entity))
    end)
  end

  def update_prompt do
    Components.all(Components.Player) |> Enum.each(fn(entity) ->
      if Entity.has_components?(entity, [Components.HP, Components.Mana]) do
        Components.Player.send_message(entity, ["update prompt", "[HP=#{Components.HP.value(entity)}/MA=#{Components.Mana.value(entity)}]:"])
      end
    end)
  end

  def hp_regen_per_second(entity) do
    (Components.Level.value(entity) + 30) * Systems.Stat.modified(entity, "health") / 300
    |> Float.floor
  end

  def mana_regen_per_second(entity) do
    (Components.Level.value(entity) + 30) * ((Systems.Stat.modified(entity, "intellect") + Systems.Stat.modified(entity, "willpower")) / 2) / 300
    |> Float.floor
  end
end
