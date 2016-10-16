defmodule ApathyDrive.Death do
  alias ApathyDrive.{Class, Mobile, Room, RoomServer, Text}

  def kill(room, mobile, killed_by \\ nil)

  def kill(%Room{} = room, victim_ref, killed_by) do
    case Room.get_mobile(room, victim_ref) do
      # Monster not possessed by a player
      %Mobile{monster_template_id: _, spirit: %Spirit{}} ->
        kill(room, victim_ref, [:drop_equipment, :reward_monster_death_exp, :unpossess, :send_home, :respawn_spirit, :delete, :convert_lair], killed_by)
      # Monster not possessed by a player
      %Mobile{spirit: nil} ->
        kill(room, victim_ref, [:reward_monster_death_exp, :generate_loot, :delete], killed_by)
    end
  end

  def kill(%Room{} = room, victim_ref, [:send_home | remaining_steps], killed_by) do
    room =
      Room.update_mobile(room, victim_ref, fn(mobile) ->
        put_in(mobile.spirit.room_id, mobile.spirit.class.start_room_id)
      end)

    kill(room, victim_ref, remaining_steps, killed_by)
  end
  def kill(%Room{} = room, victim_ref, [:drop_equipment | remaining_steps], killed_by) do
    room =
      Room.update_mobile(room, victim_ref, fn(%Mobile{spirit: %Spirit{inventory: inventory, equipment: equipment}}) ->
        room =
          room
          |> Room.add_items(inventory ++ equipment)

        update_in(room.mobiles[victim_ref].spirit, &Map.merge(&1, %{inventory: [], equipment: []}))
      end)

    kill(room, victim_ref, remaining_steps, killed_by)
  end
  def kill(%Room{} = room, victim_ref, [:reward_monster_death_exp | remaining_steps], killed_by) do
    mobile = Room.get_mobile(room, victim_ref)
    exp = ApathyDrive.Level.exp_reward(mobile.level)

    room =
      Enum.reduce(room.mobiles, room, fn
        {ref, _room_mobile}, updated_room when ref == victim_ref ->
          updated_room
        {ref, %Mobile{spirit: nil}}, updated_room ->
          Room.update_mobile(updated_room, ref, &Mobile.add_experience(&1, exp))
        {ref, %Mobile{spirit: %Spirit{}}}, updated_room ->
          Room.update_mobile(updated_room, ref, fn(room_mobile) ->
            message = mobile.death_message
                      |> Text.interpolate(%{"name" => mobile.name})
                      |> Text.capitalize_first

            Mobile.send_scroll(room_mobile, "<p>#{message}</p>")

            Mobile.send_scroll(room_mobile, "<p>You gain #{exp} essence.</p>")

            Mobile.add_experience(room_mobile, exp)
          end)
      end)

    kill(room, victim_ref, remaining_steps, killed_by)
  end
  def kill(%Room{} = room, victim_ref, [:respawn_spirit | remaining_steps], killed_by) do
    mobile = Room.get_mobile(room, victim_ref)

    send(mobile.socket, {:respawn, spirit: mobile.spirit})

    kill(room, victim_ref, remaining_steps, killed_by)
  end
  def kill(%Room{} = room, victim_ref, [:unpossess | remaining_steps], killed_by) do
    mobile = Room.get_mobile(room, victim_ref)

    Mobile.send_scroll(mobile, "<p>You leave the body of #{mobile.name}.</p>")

    message = mobile.death_message
              |> Text.interpolate(%{"name" => mobile.name})
              |> Text.capitalize_first

    Mobile.send_scroll(mobile, "<p>#{message}</p>")

    kill(room, victim_ref, remaining_steps, killed_by)
  end
  def kill(room, victim_ref, [:generate_loot | remaining_steps], killed_by) do
    room =
      Enum.reduce(room.mobiles, room, fn
      {ref, %Mobile{spirit: spirit}}, updated_room when is_nil(spirit) or ref == victim_ref ->
        updated_room
      {ref, %Mobile{spirit: %Spirit{}}}, updated_room ->
        mobile = Room.get_mobile(room, victim_ref)
        Room.update_mobile(updated_room, ref, &Mobile.generate_loot(&1, mobile.monster_template_id, Mobile.level(mobile), 50))
      end)

    kill(room, victim_ref, remaining_steps, killed_by)
  end
  def kill(room, victim_ref, [:delete | remaining_steps], killed_by) do
    Room.get_mobile(room, victim_ref)
    |> ApathyDrive.Repo.delete!()

    kill(room, victim_ref, remaining_steps, killed_by)
  end
  def kill(room, victim_ref, [], _killed_by) do
    update_in(room.mobiles, &Map.delete(&1, victim_ref))
  end

end
