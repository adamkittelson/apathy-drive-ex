defmodule ApathyDrive.LairSpawning do

  def spawn_lair(room) do
    if room.lair_size > (room.id |> Room.spawned_monsters |> length) do
      monster_templates = eligible_monsters(room)
      if Enum.any?(monster_templates) do
        monster_template = select_lair_monster(monster_templates)

        monster = MonsterTemplate.spawn_monster(monster_template, room)

        room_pid = self

        Task.start fn ->
          ApathyDrive.Exits.Normal.display_enter_message(room_pid, monster)
        end

        spawn_lair(room)
      end
    end
  end

  def select_lair_monster(monster_ids) do
    :random.seed(:os.timestamp)

    monster_ids
    |> Enum.random
  end

  def eligible_monsters(room) do
    room.lair_monsters
    |> Enum.map(&MonsterTemplate.find/1)
    |> Enum.reject(fn(mt) ->
         mt = MonsterTemplate.value(mt)

         MonsterTemplate.on_cooldown?(mt) or MonsterTemplate.limit_reached?(mt)
       end)
  end

end
