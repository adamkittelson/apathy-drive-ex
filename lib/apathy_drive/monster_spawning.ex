defmodule ApathyDrive.MonsterSpawning do
  alias ApathyDrive.{LairMonster, Monster, Repo, Room, RoomMonster}
  require Ecto.Query
  require Logger

  def load_monsters(%Room{} = room) do
    room.id
    |> monsters_to_load()
    |> Enum.reduce(room, fn room_monster, updated_room ->
      monster =
        room.mobiles
        |> Map.values()
        |> Enum.find(&(&1.room_monster_id == room_monster.id))

      # make sure the monster isn't already present -- race condition
      # with placed npcs spawning as a room loads
      if monster do
        updated_room
      else
        monster = Monster.from_room_monster(room_monster)

        Room.mobile_entered(updated_room, monster)
      end
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
    lair_monsters = LairMonster.monster_ids(room.id)
    spawn_lair(room, lair_monsters)
  end

  def spawn_zone_monster(%Room{zone_controller_id: nil} = room, _monster_ids), do: room
  def spawn_zone_monster(%Room{} = room, []), do: room

  def spawn_zone_monster(%Room{zone_controller_id: id} = room, monster_ids) do
    Process.send_after(self(), :spawn_zone_monster, 5000)

    if :rand.uniform(100) > 90 do
      monster_limit = Room.zone_monster_limit(id)

      if !zone_limit_reached?(id, monster_limit) do
        monster_id = Enum.random(monster_ids)

        if updated_room = spawn_monster(room, monster_id) do
          Logger.info("#{room.name} spawned #{monster_id} for zone #{id}")
          updated_room
        else
          room
        end
      else
        Logger.debug("#{room.name} not spawning for zone #{id} because it is full")
        room
      end
    else
      room
    end
  end

  def spawn_monster(%Room{} = room, monster_id) do
    monster =
      %RoomMonster{
        room_id: room.id,
        monster_id: monster_id,
        level: room.area.level,
        spawned_at: room.id,
        zone_spawned_at: room.zone_controller_id
      }
      |> Monster.from_room_monster()

    if monster do
      Room.mobile_entered(room, monster)
    end
  end

  defp monsters_to_load(room_id) do
    RoomMonster
    |> Ecto.Query.where([rm], rm.room_id == ^room_id and is_nil(rm.character_id))
    |> Repo.all()
  end

  defp permanent_npc_missing?(%Room{id: id, permanent_npc: monster_id}) do
    count =
      RoomMonster
      |> Ecto.Query.where(monster_id: ^monster_id, spawned_at: ^id)
      |> Ecto.Query.select([m], count(m.id))
      |> Repo.one()

    count < 1
  end

  defp spawn_lair(%Room{} = room, lair_monsters) do
    if room.lair_size > spawned_monster_count_for_room(room.id) and Enum.any?(lair_monsters) do
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

  def spawned_monster_count_for_room(room_id) do
    RoomMonster
    |> Ecto.Query.where(spawned_at: ^room_id)
    |> Ecto.Query.select([m], count(m.id))
    |> Repo.one()
  end

  def limit_reached?(%Monster{game_limit: nil}), do: false

  def limit_reached?(%Monster{id: id, game_limit: limit}) do
    count =
      RoomMonster
      |> Ecto.Query.where(monster_id: ^id)
      |> Ecto.Query.select([m], count(m.id))
      |> Repo.one()

    count >= limit
  end

  def zone_limit_reached?(_id, nil), do: true

  def zone_limit_reached?(id, limit) do
    count =
      RoomMonster
      |> Ecto.Query.where(zone_spawned_at: ^id)
      |> Ecto.Query.select([m], count(m.id))
      |> Repo.one()

    count >= limit
  end
end
