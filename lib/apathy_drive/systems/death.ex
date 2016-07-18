defmodule Systems.Death do
  alias ApathyDrive.{Class, Mobile, RoomServer, Text}

  def kill(mobile, killed_by \\ nil)

  # Player not possessing a monster
  def kill(%Mobile{monster_template_id: nil, spirit: %Spirit{}} = mobile, killed_by) do
    kill(mobile, [:drop_equipment, :inform_player, :send_home, :respawn_spirit], killed_by)
  end
  # Player possessing a monster
  def kill(%Mobile{unities: [], monster_template_id: _, spirit: %Spirit{}} = mobile, killed_by) do
    kill(mobile, [:reward_monster_death_exp, :unpossess, :respawn_spirit, :delete, :convert_lair], killed_by)
  end
  # Player possessing a turned monster
  def kill(%Mobile{monster_template_id: _, spirit: %Spirit{}} = mobile, killed_by) do
    kill(mobile, [:reward_monster_death_exp, :unpossess, :respawn_spirit, :delete, :convert_lair], killed_by)
  end
  # Monster not possessed by a player
  def kill(%Mobile{unities: [], spirit: nil} = mobile, killed_by) do
    kill(mobile, [:reward_monster_death_exp, :generate_loot, :delete, :convert_lair], killed_by)
  end
  # Turned monster not possessed by a player
  def kill(%Mobile{spirit: nil} = mobile, killed_by) do
    kill(mobile, [:reward_monster_death_exp, :generate_loot, :delete, :convert_lair], killed_by)
  end

  def kill(mobile, [:send_home | remaining_steps], killed_by) do
    mobile =
      put_in(mobile.spirit.room_id, mobile.spirit.class.start_room_id)

    kill(mobile, remaining_steps, killed_by)
  end
  def kill(%Mobile{spirit: %Spirit{inventory: inventory, equipment: equipment} = spirit} = mobile, [:drop_equipment | remaining_steps], killed_by) do
    RoomServer.add_items({:global, "room_#{mobile.room_id}"}, inventory ++ equipment)

    spirit =
      Map.merge(spirit, %{inventory: [], equipment: []})

    mobile = Map.put(mobile, :spirit, spirit)

    kill(mobile, remaining_steps, killed_by)
  end
  def kill(mobile, [:reward_monster_death_exp | remaining_steps], killed_by) do
    ApathyDrive.PubSub.broadcast!("rooms:#{mobile.room_id}:mobiles", {:mobile_died, mobile: mobile, reward: ApathyDrive.Level.exp_reward(mobile.level)})

    kill(mobile, remaining_steps, killed_by)
  end
  def kill(mobile, [:inform_player | remaining_steps], killed_by) do
    send(mobile.socket, {:scroll, "<p><span class='red'>You have been killed!</span></p>"})
    ApathyDrive.Endpoint.broadcast "rooms:#{mobile.room_id}:mobiles", "scroll", %{:html => "<p><span class='red'>#{mobile.name} has been killed!</span></p>"}

    kill(mobile, remaining_steps, killed_by)
  end
  def kill(mobile, [:respawn_spirit | remaining_steps], killed_by) do
    send(mobile.socket, {:respawn, spirit: mobile.spirit})

    kill(mobile, remaining_steps, killed_by)
  end
  def kill(mobile, [:unpossess | remaining_steps], killed_by) do
    Process.unregister(:"spirit_#{mobile.spirit.id}")
    send(mobile.socket, {:scroll, "<p>You leave the body of #{mobile.name}.</p>"})

    message = mobile.death_message
              |> Text.interpolate(%{"name" => mobile.name})
              |> Text.capitalize_first

    send(mobile.socket, {:scroll, "<p>#{message}</p>"})

    kill(mobile, remaining_steps, killed_by)
  end
  def kill(mobile, [:generate_loot | remaining_steps], killed_by) do
    ApathyDrive.PubSub.broadcast!("rooms:#{mobile.room_id}:spirits", {:generate_loot, mobile.monster_template_id, Mobile.level(mobile), 50})

    kill(mobile, remaining_steps, killed_by)
  end
  def kill(mobile, [:delete | remaining_steps], killed_by) do
    ApathyDrive.Repo.delete!(mobile)

    kill(mobile, remaining_steps, killed_by)
  end
  def kill(mobile, [:convert_lair | remaining_steps], nil) do
    kill(mobile, remaining_steps, nil)
  end
  def kill(mobile, [:convert_lair | remaining_steps], %Mobile{spirit: nil, unities: [unity]} = killed_by) do
    convert_lair(mobile, unity)

    kill(mobile, remaining_steps, killed_by)
  end
  def kill(mobile, [:convert_lair | remaining_steps], %Mobile{spirit: %Spirit{class: %Class{unities: [unity]}}} = killed_by) do
    convert_lair(mobile, unity)

    kill(mobile, remaining_steps, killed_by)
  end
  def kill(mobile, [:convert_lair | remaining_steps], killed_by) do
    kill(mobile, remaining_steps, killed_by)
  end
  def kill(mobile, [], _killed_by) do
    mobile
  end

  defp convert_lair(%{spawned_at: room_id}, unity) do
    IO.puts "maybe converting room #{room_id} for #{unity}"

    room_id
    |> RoomServer.find
    |> RoomServer.convert?(unity)
  end
  defp convert_lair(_mobile, _unity), do: :noop

end
