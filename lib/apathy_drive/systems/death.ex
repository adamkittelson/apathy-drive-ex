defmodule Systems.Death do
  use Systems.Reload
  import Utility
  import Systems.Text
  use Timex

  def kill(entity) do
    room = Parent.of(entity)

    room
    |> Systems.Room.characters_in_room
    |> Enum.each(fn(character) ->

         monster = Possession.possessed(character)

         if monster == entity do
           send_message(character, "scroll", "<p><span class='red'>You have been killed!</span></p>")
         else
           send_message(character, "scroll", "<p>#{death_message(entity)}</p>")
           reward_player(monster, entity)
         end
       end)

    corpse = create_corpse(entity, room)

    kill_monster(entity, corpse, room)
  end

  def kill_monster(entity, corpse, room) do
    HPRegen.remove(entity)
    ManaRegen.remove(entity)
    Components.Effects.remove(entity)
    possessor = Possession.possessor(entity)
    if possessor do
      Possession.unpossess(entity)
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

    Components.Monsters.remove_monster(room, entity)
    Components.Combat.stop_timer(entity)
    Entity.list_components(entity) |> Enum.each(&(Entity.remove_component(entity, &1)))
    GenEvent.stop(entity)
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

  def reward_player(entity, deceased) do
    exp = experience_to_grant(deceased)
    Components.Experience.add(entity, exp)
    send_message(entity, "scroll", "<p>You gain #{exp} experience.")
  end
end
