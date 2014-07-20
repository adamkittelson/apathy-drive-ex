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
         if character == entity do
           send_message(character, "scroll", "<p><span class='red'>You have been killed!</span></p>")
         else
           send_message(character, "scroll", "<p>#{death_message(entity)}</p>")
           if !Components.Spirit.value(character) do
             reward_player(character, entity)
           end
         end
       end)

    create_corpse(entity, room)

    if Entity.has_component?(entity, Components.Spirit) do
      kill_player(entity)
    else
      kill_monster(entity, room)
    end
  end

  def kill_player(entity) do
    HPRegen.remove(entity)
    ManaRegen.remove(entity)
    Entity.remove_component(entity, Components.Race)
    Entity.remove_component(entity, Components.Stats)
    Entity.remove_component(entity, Components.Gender)
    Entity.remove_component(entity, Components.EyeColor)
    Entity.remove_component(entity, Components.HairColor)
    Entity.remove_component(entity, Components.HairLength)
    Entity.remove_component(entity, Components.HP)
    Entity.remove_component(entity, Components.Mana)
    Entity.remove_component(entity, Components.Limbs)
    Entity.remove_component(entity, Components.Hunting)
    Entity.remove_component(entity, Components.Attacks)
    Components.Skills.value(entity, %{})
    Components.Spirit.value(entity, true)
    Entities.save!(entity)
    send_message(entity, "scroll", "<p>Your are a spirit once more.</p>")
  end

  def kill_monster(entity, room) do
    HPRegen.remove(entity)
    ManaRegen.remove(entity)
    Components.Monsters.remove_monster(room, entity)
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
    Entity.add_component(corpse, Components.Decay, %{"frequency" => 1, "decay_at" => Date.convert(Date.shift(Date.now, mins: 1), :secs)})
    Entities.save!(corpse)

    Components.Items.add_item(room, corpse)

    Entities.save!(room)
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
    Systems.Stat.modified(entity)
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
