defmodule ApathyDrive.LairSpawning do
  alias ApathyDrive.Mobile

  def spawn_lair(room) do
    # lair_monsters = ApathyDrive.LairMonster.monsters_template_ids(room.id)
    # 
    # if room.lair_size > (room.id |> Room.spawned_monsters |> length) do
    #   monster_templates = eligible_monsters(lair_monsters)
    #   if Enum.any?(monster_templates) do
    #     {mt_id, monster_template} = select_lair_monster(monster_templates)
    # 
    #     monster = MonsterTemplate.create_monster(monster_template, room)
    #               |> Mobile.load
    # 
    #     ApathyDrive.PubSub.broadcast!("rooms:#{room.id}:adjacent", {:audible_movement, room.id, nil})
    # 
    #     Mobile.display_enter_message(monster, self)
    # 
    #     spawn_lair(room)
    #   end
    # end
  end

  def select_lair_monster(monster_ids) do
    :random.seed(:os.timestamp)

    monster_ids
    |> Enum.random
  end

  def eligible_monsters(lair_monsters) do
    lair_monsters
    |> Enum.map(&({&1, MonsterTemplate.find(&1)}))
    |> Enum.reject(fn({_mt_id, mt}) ->
         mt = MonsterTemplate.value(mt)

         MonsterTemplate.on_cooldown?(mt) or MonsterTemplate.limit_reached?(mt)
       end)
  end

end
