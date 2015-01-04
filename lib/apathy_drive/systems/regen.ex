defmodule Systems.Regen do
  use Systems.Reload
  import Utility
  import BlockTimer
  import Systems.Text

  def initialize_regen(monster) do
    if !Components.Regenerating.value(monster) do
      Components.Regenerating.value(monster, true)

      Components.TimerManager.call_after(monster, {:regen, 5000, fn ->
        hp = hp_regen_per_tick(monster)
        Components.HP.add(monster, hp)
        heal_limbs(monster, hp)
        update_prompt(monster)

        Components.Mana.add(monster, mana_regen_per_tick(monster))

        Components.Regenerating.value(monster, false)

        if !fully_healed?(monster) or Components.Mana.value(monster) < Systems.Mana.max(monster) do
          initialize_regen(monster)
        end
      end})
    end
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
      |> Enum.each(fn(spirit) ->
           observer = Possession.possessed(spirit) || spirit

           cond do
             observer == entity ->
               send_message(observer, "scroll", "<p>Your #{limb} no longer crippled!</p>")
              true ->
               send_message(observer, "scroll", "<p>#{Components.Name.value(entity) |> capitalize_first}'s #{limb} is no longer crippled!</p>")
           end
         end)
    end
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
