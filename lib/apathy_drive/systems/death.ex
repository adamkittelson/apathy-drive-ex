defmodule Systems.Death do
  use Systems.Reload
  import Utility
  import Systems.Text
  use Timex

  def kill(victim) do
    room = Parent.of(victim)

    send_message(victim, "scroll", "<p><span class='red'>You have been killed!</span></p>")

    room
    |> Systems.Monster.monsters_in_room(victim)
    |> Enum.each(fn(monster) ->
         send_message(monster, "scroll", "<p>#{death_message(victim)}</p>")
         reward_monster(monster, victim)
         reward_spirit(Possession.possessor(monster), victim)
       end)

    corpse = create_corpse(victim, room)

    kill_monster(victim, corpse, room)
  end

  def kill_monster(entity, corpse, room) do
    HPRegen.remove(entity)
    ManaRegen.remove(entity)
    Components.Effects.remove(entity)
    possessor = Possession.possessor(entity)
    if possessor do
      Possession.unpossess(possessor)
      Systems.Prompt.update(entity, nil)
      send_message(possessor, "scroll", "<p>Your are a spirit once more.</p>")
    end

    Systems.Limbs.equipped_items(entity)
    |> Enum.each fn(item) ->
         Components.Limbs.unequip(entity, item)
         Components.Items.add_item(corpse, item)
       end

    Components.Items.get_items(entity)
    |> Enum.each fn(item) ->
         Components.Items.remove_item(entity, item)
         Components.Items.add_item(corpse, item)
       end

    Entities.save!(corpse)

    Components.Investments.list(entity)
    |> Enum.map(&(&1 |> String.to_integer |> Characters.find_by_id))
    |> Enum.each(&(Components.Investments.uninvest(&1, Components.ID.value(entity))))

    Components.Monsters.remove_monster(room, entity)
    Components.Combat.stop_timer(entity)
    Entities.delete!(entity)
  end

  def create_corpse(entity, room) do
    {:ok, corpse} = Entity.init
    Entity.add_component(corpse, Components.Name,        "the corpse of #{Components.Name.value(entity)}")
    Entity.add_component(corpse, Components.Description, "This is the dead body of #{Components.Name.value(entity)}")
    if Entity.has_component?(entity, Components.Module) do
      Entity.add_component(corpse, Components.Module, Components.Module.value(entity))
    end
    Entity.add_component(corpse, Components.Types, ["item", "corpse"])
    Entity.add_component(corpse, Components.Items, [])
    Entity.add_component(corpse, Components.Decay, %{"frequency" => 1, "decay_at" => Date.convert(Date.shift(Date.now, mins: 1), :secs)})
    Entities.save!(corpse)

    Components.Items.add_item(room, corpse)

    Entities.save!(room)
    corpse
  end

  def death_message(entity) do
    default = "#{capitalize_first(Components.Name.value(entity))} drops <span class='dark-red'>dead</span> before you."
    if Entity.has_component?(entity, Components.Module) do
      Components.Module.value(entity).properties[:death_message] || default
    else
      default
    end
  end

  def experience_to_grant(entity) when is_pid entity do
    Systems.Stat.pre_effects_bonus(entity)
    |> Map.values
    |> Enum.sum
    |> experience_to_grant
  end

  def experience_to_grant(stat_total) do
    trunc(stat_total * (1 + (stat_total * 0.005)))
  end

  def reward_monster(monster, victim) do
    exp = experience_to_grant(victim)
    old_power = Systems.Trainer.total_power(monster)
    Components.Experience.add(monster, exp)
    new_power = Systems.Trainer.total_power(monster)
    power_gain = new_power - old_power
    if power_gain > 0 do
      send_message(monster, "scroll", "<p>Your #{Components.Name.value(monster)} gains #{power_gain} power.</p>")
    end
  end

  def reward_spirit(nil, victim), do: nil
  def reward_spirit(spirit, victim) do
    exp = experience_to_grant(victim)
    old_power = Systems.Trainer.total_power(spirit)
    Components.Experience.add(spirit, exp)
    new_power = Systems.Trainer.total_power(spirit)
    power_gain = new_power - old_power
    if power_gain > 0 do
      send_message(spirit, "scroll", "<p>You gain #{power_gain} power.</p>")
    end
  end
end
