defmodule ApathyDrive.MonsterSpawning do
  alias ApathyDrive.{LairMonster, Monster, Repo, Room, RoomMonster}
  require Ecto.Query

  def load_monsters(%Room{} = room) do
    room.id
    |> monsters_to_load()
    |> Enum.reduce(room, fn(room_monster, updated_room) ->
         monster = Monster.from_room_monster(room_monster)

         Room.mobile_entered(updated_room, monster)
       end)
  end

  def spawn_permanent_npc(%Room{permanent_npc: nil} = room), do: room
  def spawn_permanent_npc(%Room{permanent_npc: monster_id} = room) do
    if permanent_npc_missing?(room) do
      spawn_monster(room, monster_id) || room
    else
      room
    end
  end

  def spawn_lair(%Room{} = room) do
    lair_monsters = LairMonster.monsters_template_ids(room.id)
    spawn_lair(room, lair_monsters)
  end

  def spawn_monster(%Room{} = room, monster_id) do
    monster =
      %RoomMonster{
        room_id: room.id,
        monster_id: monster_id,
        level: room.area.level,
        spawned_at: room.id
      }
      |> Monster.from_room_monster

    if monster do
      Room.mobile_entered(room, monster)
    end
  end

  defp monsters_to_load(room_id) do
    RoomMonster
    |> Ecto.Query.where(room_id: ^room_id)
    |> Repo.all
  end

  defp permanent_npc_missing?(%Room{permanent_npc: monster_id} = room) do
    room.mobiles
    |> Map.values
    |> Enum.all?(&(&1.id != monster_id))
  end

  defp spawn_lair(%Room{} = room, lair_monsters) do
    if room.lair_size > spawned_monster_count(room.id) and Enum.any?(lair_monsters) do
      monster_id = Enum.random(lair_monsters)

      if updated_room = spawn_monster(room, monster_id) do
        spawn_lair(updated_room, lair_monsters)
      else
        room
      end
    else
      room
    end
  end

  def spawned_monster_count(room_id) do
    RoomMonster
    |> Ecto.Query.where(spawned_at: ^room_id)
    |> Ecto.Query.select([m], count(m.id))
    |> Repo.one
  end

end
