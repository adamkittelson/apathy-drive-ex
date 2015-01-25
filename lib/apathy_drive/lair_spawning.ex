defmodule ApathyDrive.LairSpawning do
  use Systems.Reload

  def spawn_lair(room) do
    if room.lair_size > (room.id |> Room.spawned_monsters |> length) do
      monster_template_ids = eligible_monsters(room)
      if Enum.any?(monster_template_ids) do
        monster_template = select_lair_monster(monster_template_ids)

        monster = MonsterTemplate.spawn_monster(monster_template, room)

        Monster.lair_id(monster, room.id)

        Phoenix.PubSub.subscribe(monster, "rooms:#{room.id}:spawned_monsters")
        Phoenix.PubSub.subscribe(monster, "monster_template:#{MonsterTemplate.id(monster_template)}")
        Monster.display_enter_message(room, monster)
        spawn_lair(room)
      end
    end
  end

  def select_lair_monster(monster_ids) do
    :random.seed(:os.timestamp)

    monster_ids
    |> Enum.shuffle
    |> List.first
  end

  def eligible_monsters(room) do
    room.lair_monsters
    |> Enum.map(&MonsterTemplate.find/1)
    |> Enum.reject(&MonsterTemplate.limit_reached?/1)
  end

end
