defmodule Systems.Regen do
  use Systems.Reload
  import Utility

  def initialize do
    every 1, do: regen
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
        Systems.Prompt.update(entity)
      end
    end)
  end

  def regen_rate(seed) when is_integer(seed) do
    regen_rate(seed, 0)
  end

  def regen_rate(seed, rate) when seed > 0 do
    regen_rate(seed - 1, rate + ((seed - 1) / 100))
  end

  def regen_rate(0, rate) do
    Float.floor(rate)
  end

  def hp_regen_per_second(entity) do
    regen_rate(Systems.Stat.modified(entity, "health"))
  end

  def mana_regen_per_second(entity) do
    intellect = Systems.Stat.modified(entity, "intellect")
    willpower = Systems.Stat.modified(entity, "willpower")
    regen_rate(Float.floor((intellect + willpower * 2) / 3))
  end
end
