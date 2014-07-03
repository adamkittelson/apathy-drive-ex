defmodule Systems.LairSpawning do
  use Systems.Reload
  import Utility

  def initialize do
    every 10, do: spawn_lairs
  end

  def spawn_lairs do
    :random.seed(:erlang.now)
    Components.all(Components.Lair) |> Enum.each(fn(room) ->
      {mega, seconds, _} = :os.timestamp
      spawn_at = (mega * 1000000 + seconds) - Components.Lair.frequency(room) * 60
      case Components.Lair.last_spawned_at(room) do
        nil ->
          spawn_lair(room)
        {mega, seconds, _} when (mega * 1000000 + seconds) <= spawn_at ->
          spawn_lair(room)
        _ ->
      end
    end)
  end

  def spawn_lair(room) do
    Components.Lair.set_last_spawned_at(room)
    if (Systems.Room.monsters_in_room(room) |> Enum.count) < Components.Lair.size(room) do
      room |> select_lair_monster
           |> Systems.Monster.spawn_monster(room)
      spawn_lair(room)
    end
  end

  def select_lair_monster(room) do
    room |> Components.Lair.monster_templates
         |> Enum.shuffle
         |> List.first
  end

end
