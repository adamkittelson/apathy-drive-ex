defmodule ApathyDrive.Death do
  alias ApathyDrive.{Monster, Room, Text}

  def kill(room, monster, killed_by \\ nil)

  def kill(%Room{} = room, victim_ref, killed_by) do
    case Room.get_monster(room, victim_ref) do
      # Monster not possessed by a player
      %Monster{monster_template_id: _, spirit: %Spirit{}} ->
        kill(room, victim_ref, [:drop_equipment, :reward_monster_death_exp, :unpossess, :send_home, :respawn_spirit, :delete, :convert_lair], killed_by)
      # Monster not possessed by a player
      %Monster{spirit: nil} ->
        kill(room, victim_ref, [:reward_monster_death_exp, :generate_loot, :delete], killed_by)
    end
  end

  def kill(%Room{} = room, victim_ref, [:send_home | remaining_steps], killed_by) do
    room =
      Room.update_monster(room, victim_ref, fn(monster) ->
        put_in(monster.spirit.room_id, monster.spirit.class.start_room_id)
      end)

    kill(room, victim_ref, remaining_steps, killed_by)
  end
  def kill(%Room{} = room, victim_ref, [:drop_equipment | remaining_steps], killed_by) do
    room =
      Room.update_monster(room, victim_ref, fn(%Monster{spirit: %Spirit{inventory: inventory, equipment: equipment}}) ->
        room =
          room
          |> Room.add_items(inventory ++ equipment)

        update_in(room.monsters[victim_ref].spirit, &Map.merge(&1, %{inventory: [], equipment: []}))
      end)

    kill(room, victim_ref, remaining_steps, killed_by)
  end
  def kill(%Room{} = room, victim_ref, [:reward_monster_death_exp | remaining_steps], killed_by) do
    monster = Room.get_monster(room, victim_ref)
    exp = ApathyDrive.Level.exp_reward(monster.level)

    room =
      Enum.reduce(room.monsters, room, fn
        {ref, _room_monster}, updated_room when ref == victim_ref ->
          updated_room
        {ref, %Monster{spirit: nil}}, updated_room ->
          Room.update_monster(updated_room, ref, &Monster.add_experience(&1, exp))
        {ref, %Monster{spirit: %Spirit{}}}, updated_room ->
          Room.update_monster(updated_room, ref, fn(room_monster) ->
            message = monster.death_message
                      |> Text.interpolate(%{"name" => monster.name})
                      |> Text.capitalize_first

            Monster.send_scroll(room_monster, "<p>#{message}</p>")

            Monster.send_scroll(room_monster, "<p>You gain #{exp} essence.</p>")

            Monster.add_experience(room_monster, exp)
          end)
      end)

    kill(room, victim_ref, remaining_steps, killed_by)
  end
  def kill(%Room{} = room, victim_ref, [:respawn_spirit | remaining_steps], killed_by) do
    monster = Room.get_monster(room, victim_ref)

    send(monster.socket, {:respawn, spirit: monster.spirit})

    kill(room, victim_ref, remaining_steps, killed_by)
  end
  def kill(%Room{} = room, victim_ref, [:unpossess | remaining_steps], killed_by) do
    monster = Room.get_monster(room, victim_ref)

    Monster.send_scroll(monster, "<p>You leave the body of #{monster.name}.</p>")

    message = monster.death_message
              |> Text.interpolate(%{"name" => monster.name})
              |> Text.capitalize_first

    Monster.send_scroll(monster, "<p>#{message}</p>")

    kill(room, victim_ref, remaining_steps, killed_by)
  end
  def kill(room, victim_ref, [:generate_loot | remaining_steps], killed_by) do
    room =
      Enum.reduce(room.monsters, room, fn
      {ref, %Monster{spirit: spirit}}, updated_room when is_nil(spirit) or ref == victim_ref ->
        updated_room
      {ref, %Monster{spirit: %Spirit{}}}, updated_room ->
        monster = Room.get_monster(room, victim_ref)
        Room.update_monster(updated_room, ref, &Monster.generate_loot(&1, monster.monster_template_id, Monster.level(monster), 50))
      end)

    kill(room, victim_ref, remaining_steps, killed_by)
  end
  def kill(room, victim_ref, [:delete | remaining_steps], killed_by) do
    Room.get_monster(room, victim_ref)
    |> ApathyDrive.Repo.delete!()

    kill(room, victim_ref, remaining_steps, killed_by)
  end
  def kill(room, victim_ref, [], _killed_by) do
    update_in(room.monsters, &Map.delete(&1, victim_ref))
  end

end
