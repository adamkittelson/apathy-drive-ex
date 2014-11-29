defmodule Systems.Regen do
  use Systems.Reload
  import Utility
  import BlockTimer
  import Systems.Text

  def initialize do
    ["HP", "Mana"]
    |> Enum.each(fn(type) ->
         Components.all(:"Elixir.Components.#{type}")
         |> Enum.each(fn(entity) ->
              if :"Elixir.Components.#{type}".value(entity) < :"Elixir.Systems.#{type}".max(entity) do
                :"Elixir.#{type}Regen".add(entity)
              end
            end)
       end)

    apply_interval 5 |> seconds, do: regen_hp
    apply_interval 5 |> seconds, do: regen_mana
  end

  def regen_hp do
    HPRegen.all
    |> Enum.each(fn(entity) ->
         if Process.alive?(entity) do
           hp = hp_regen_per_tick(entity)
           Components.HP.add(entity, hp)
           heal_limbs(entity, hp)
           if fully_healed?(entity) do
             HPRegen.remove(entity)
           end
           update_prompt(entity)
         end
       end)
  end

  def fully_healed?(entity) do
    (Components.HP.value(entity) >= Systems.HP.max(entity)) && !Components.Limbs.injured?(entity)
  end

  def heal_limbs(entity, hp) do
    limbs = Components.Limbs.unsevered_limbs(entity)
    amount = Float.ceil(hp / length(limbs)) |> trunc
    limbs
    |> Enum.each &(heal_limb(entity, &1, amount))
  end

  def heal_limb(entity, limb, amount) do
    crippled = Components.Limbs.crippled?(entity, limb)
    Components.Limbs.heal_limb(entity, limb, amount)
    if crippled && !Components.Limbs.crippled?(entity, limb) do
      Parent.of(entity)
      |> Systems.Room.characters_in_room
      |> Enum.each(fn(character) ->
           cond do
             character == entity ->
               send_message(character, "scroll", "<p>Your #{limb} no longer crippled!</p>")
              true ->
               send_message(character, "scroll", "<p>#{Components.Name.value(entity) |> capitalize_first}'s #{limb} is no longer crippled!</p>")
           end
         end)
    end
  end

  def regen_mana do
    ManaRegen.all
    |> Enum.each(fn(entity) ->
         if Process.alive?(entity) do
           Components.Mana.add(entity, mana_regen_per_tick(entity))
           if Components.Mana.value(entity) >= Systems.Mana.max(entity) do
             ManaRegen.remove(entity)
           end
           update_prompt(entity)
         end
       end)
  end

  def update_prompt(entity) do
    entity
    |> Possession.possessor
    |> Systems.Prompt.update(entity)
  end

  def regen_rate(seed) when is_integer(seed) do
    regen_rate(trunc(seed / 2), 0)
  end

  def regen_rate(seed, rate) when seed > 0 do
    regen_rate(seed - 1, rate + ((seed - 1) / 100))
  end

  def regen_rate(0, rate) do
    trunc(rate)
  end

  def hp_regen_per_tick(entity) do
    regen_rate(Systems.Stat.modified(entity, "health"))
    |> max(1)
  end

  def mana_regen_per_tick(entity) do
    intellect = Systems.Stat.modified(entity, "intellect")
    willpower = Systems.Stat.modified(entity, "willpower")
    regen_rate(trunc((intellect + willpower * 2) / 3))
    |> max(1)
  end
end
