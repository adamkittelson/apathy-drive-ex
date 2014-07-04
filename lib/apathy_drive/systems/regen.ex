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
      if Components.HP.value(entity) < Systems.HP.max_hp(entity) do
        hp = hp_regen_per_second(entity)
        Components.HP.add(entity, hp)
        heal_limbs(entity, hp)
      end
    end)
  end

  def heal_limbs(entity, hp) do
    limbs = Components.Limbs.unsevered_limbs(entity)
    amount = Float.ceil(hp / length(limbs))
    limbs
    |> Enum.each &(heal_limb(entity, &1, amount))
  end

  def heal_limb(entity, limb, amount) do
    crippled = Components.Limbs.crippled?(entity, limb)
    Components.Limbs.heal_limb(entity, limb, amount)
    if crippled && !Components.Limbs.crippled?(entity, limb) do
      Components.CurrentRoom.get_current_room(entity)
      |> Systems.Room.characters_in_room
      |> Enum.each(fn(character) ->
           cond do
             character == entity ->
               send_message(character, "scroll", "<p>Your #{limb} no longer crippled!</p>")
              true ->
               send_message(character, "scroll", "<p>#{Components.Name.value(entity)}'s #{limb} is no longer crippled!</p>")
           end
         end)
    end
  end

  def regen_mana do
    Components.all(Components.Mana) |> Enum.each(fn(entity) ->
      if Components.Mana.value(entity) < Systems.Mana.max_mana(entity) do
        Components.Mana.add(entity, mana_regen_per_second(entity))
      end
    end)
  end

  def update_prompt do
    Components.all(Components.Socket) |> Enum.each(fn(entity) ->
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
