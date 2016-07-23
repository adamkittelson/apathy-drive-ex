defmodule ApathyDrive.LairSpawning do
  alias ApathyDrive.{MonsterTemplate, Mobile, Repo, Room}
  require Ecto.Query

  def spawn_lair(%Room{} = room) do
    lair_monsters = ApathyDrive.LairMonster.monsters_template_ids(room.id)

    spawn_lair(room, lair_monsters)
  end

  defp spawn_lair(room, lair_monsters) do
    if room.lair_size > spawned_monster_count(room.id) do
      monster_templates = eligible_monsters(lair_monsters)
      if Enum.any?(monster_templates) do
        {mt_id, monster_template} = select_lair_monster(monster_templates)

        monster =
          MonsterTemplate.create_monster(monster_template, room)
          |> Mobile.init

        Room.audible_movement(room, nil)

        Room.display_enter_message(room, monster)

        room = put_in(room.mobiles[monster.ref], monster)

        spawn_lair(room, lair_monsters)
      else
        room
      end
    else
      room
    end
  end

  def select_lair_monster(monster_ids) do
    monster_ids
    |> Enum.random
  end

  def eligible_monsters(lair_monsters) do
    lair_monsters
    |> Enum.map(&({&1, MonsterTemplate.find(&1)}))
    |> Enum.reject(fn({_mt_id, mt}) ->
         mt = MonsterTemplate.value(mt)

         MonsterTemplate.limit_reached?(mt)
       end)
  end

  def spawned_monster_count(room_id) do
    ApathyDrive.Mobile
    |> Ecto.Query.where(spawned_at: ^room_id)
    |> Ecto.Query.select([m], count(m.id))
    |> Repo.one
  end

end
