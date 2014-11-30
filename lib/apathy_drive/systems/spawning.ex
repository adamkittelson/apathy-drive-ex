defmodule Systems.Spawning do
  use Systems.Reload
  import BlockTimer
  use Timex

  def initialize do
    apply_interval 10 |> seconds, do: spawn_lairs
    apply_interval 10 |> seconds, do: spawn_permanent_npcs
    apply_interval 10 |> seconds, do: spawn_placed_items
  end

  def spawn_lairs do
    :random.seed(:erlang.now)
    Components.all(Components.Lair) |> Enum.each(fn(room) ->
      last_spawned_at = Components.Lair.last_spawned_at(room)
      if last_spawned_at do
        spawn_at = last_spawned_at
                   |> Date.shift(mins: Components.Lair.frequency(room))
                   |> Date.convert :secs

        if spawn_at < Date.convert(Date.now, :secs) do
          spawn_lair(room)
        end
      else
        spawn_lair(room)
      end
    end)
  end

  def spawn_permanent_npcs do
    Components.all(Components.PermanentNPC) |> Enum.each(fn(room) ->
      if !permanent_npc_present?(room) do
        monster = MonsterTemplates.find_by_id(Components.PermanentNPC.value(room))
        if monster do
          Systems.Monster.spawn_monster(monster, room)
        end
      end
    end)
  end

  def permanent_npc_present?(room) do
    Systems.Room.monsters_in_room(room)
    |> Enum.map(fn(monster) ->
         Components.Name.value(monster)
       end)
    |> Enum.member?(Components.PermanentNPC.value(room))
  end

  def spawn_placed_items do
    Components.all(Components.PlacedItems) |> Enum.each(fn(room) ->
      Components.PlacedItems.value(room)
      |> Enum.each(fn(item_name) ->
           if !placed_item_present?(room, item_name) do
             item = ItemTemplates.find_by_id(item_name)
             if item do
               Systems.Item.spawn_item(item, room)
             end
           end
         end)
    end)
  end

  def placed_item_present?(room, item_name) do
    Components.Items.get_items(room)
    |> Enum.any?(fn(item) ->
         Components.Name.value(item) == item_name
       end)
  end

  def spawn_lair(room) do
    Components.Lair.set_last_spawned_at(room)
    if (Systems.Room.monsters_in_room(room) |> Enum.count) < Components.Lair.size(room) and Enum.any?(eligible_monsters(room)) do
      room |> select_lair_monster
           |> Systems.Monster.spawn_monster(room)
      spawn_lair(room)
    end
  end

  def select_lair_monster(room) do
    room |> eligible_monsters
         |> Enum.shuffle
         |> List.first
  end

  def eligible_monsters(room) do
    room
    |> Components.Lair.monster_templates
    |> Enum.reject(fn(mt) ->
         monster_name = Components.Name.value(mt)
         limit = Components.Module.value(mt).limit
         Systems.Monster.limit_reached?(monster_name, limit)
       end)
  end

end
