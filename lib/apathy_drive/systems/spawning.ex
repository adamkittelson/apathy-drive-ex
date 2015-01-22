defmodule Systems.Spawning do
  use Systems.Reload
  import BlockTimer
  use Timex

  def initialize do
    apply_interval 10 |> seconds, do: spawn_permanent_npcs
    apply_interval 10 |> seconds, do: spawn_placed_items
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
    |> Enum.any?(fn(monster_name) ->
         String.contains?(monster_name, Components.PermanentNPC.value(room))
       end)
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

end
