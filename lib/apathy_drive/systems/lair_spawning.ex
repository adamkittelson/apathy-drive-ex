defmodule Systems.LairSpawning do
  use Systems.Reload
  import BlockTimer
  use Timex

  def initialize do
    apply_interval 10 |> seconds, do: spawn_lairs
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
