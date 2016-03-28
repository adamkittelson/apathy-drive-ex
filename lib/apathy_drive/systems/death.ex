defmodule Systems.Death do
  alias ApathyDrive.Mobile

  # Player not possessing a monster
  def kill(%Mobile{monster_template_id: nil, spirit: %Spirit{}} = mobile) do
    kill(mobile, [:drop_equipment, :inform_player, :send_home, :respawn_spirit])
  end
  # Player possessing a monster
  def kill(%Mobile{unity: nil, monster_template_id: _, spirit: %Spirit{}} = mobile) do
    kill(mobile, [:reward_monster_death_exp, :respawn_spirit, :unpossess, :set_last_killed_at, :delete])
  end
  # Player possessing a turned monster
  def kill(%Mobile{monster_template_id: _, spirit: %Spirit{}} = mobile) do
    kill(mobile, [:reward_monster_death_exp, :respawn_spirit, :unpossess, :set_last_killed_at, :delete])
  end
  # Monster not possessed by a player
  def kill(%Mobile{unity: nil, spirit: nil} = mobile) do
    kill(mobile, [:reward_monster_death_exp, :generate_loot, :set_last_killed_at, :delete])
  end
  # Turned monster not possessed by a player
  def kill(%Mobile{spirit: nil} = mobile) do
    kill(mobile, [:reward_monster_death_exp, :generate_loot, :set_last_killed_at, :delete])
  end

  def kill(mobile, [:send_home | remaining_steps]) do
    mobile =
      put_in(mobile.spirit.room_id, mobile.spirit.class.start_room_id)

    kill(mobile, remaining_steps)
  end
  def kill(%Mobile{spirit: %Spirit{inventory: inventory, equipment: equipment} = spirit} = mobile, [:drop_equipment | remaining_steps]) do
    mobile.room_id
    |> Room.find
    |> Room.add_items(inventory ++ equipment)

    spirit =
      Map.merge(spirit, %{inventory: [], equipment: []})

    mobile = Map.put(mobile, :spirit, spirit)

    kill(mobile, remaining_steps)
  end
  def kill(mobile, [:reward_monster_death_exp | remaining_steps]) do
    recipient_count = length(ApathyDrive.PubSub.subscribers("rooms:#{mobile.room_id}:mobiles", [self]))

    if recipient_count > 0 do
      reward = div(ApathyDrive.Level.exp_reward(mobile.level), recipient_count)

      ApathyDrive.PubSub.broadcast!("rooms:#{mobile.room_id}:mobiles", {:mobile_died, mobile: mobile, reward: reward})
    end

    kill(mobile, remaining_steps)
  end
  def kill(mobile, [:inform_player | remaining_steps]) do
    send(mobile.socket, {:scroll, "<p><span class='red'>You have been killed!</span></p>"})
    ApathyDrive.Endpoint.broadcast "rooms:#{mobile.room_id}:mobiles", "scroll", %{:html => "<p><span class='red'>#{mobile.name} has been killed!</span></p>"}

    kill(mobile, remaining_steps)
  end
  def kill(mobile, [:respawn_spirit | remaining_steps]) do
    send(mobile.socket, {:respawn, spirit: mobile.spirit})

    kill(mobile, remaining_steps)
  end
  def kill(mobile, [:unpossess | remaining_steps]) do
    send(mobile.socket, {:scroll, "<p>You leave the body of #{mobile.name}.</p>"})

    message = mobile.death_message
              |> Systems.Text.interpolate(%{"name" => mobile.name})
              |> Systems.Text.capitalize_first

    send(mobile.socket, {:scroll, "<p>#{message}</p>"})

    kill(mobile, remaining_steps)
  end
  def kill(mobile, [:generate_loot | remaining_steps]) do
    ApathyDrive.PubSub.broadcast!("rooms:#{mobile.room_id}:spirits", {:generate_loot, mobile.monster_template_id, Mobile.level(mobile)})

    kill(mobile, remaining_steps)
  end
  def kill(mobile, [:set_last_killed_at | remaining_steps]) do
    MonsterTemplate.set_last_killed_at(mobile)

    kill(mobile, remaining_steps)
  end
  def kill(mobile, [:delete | remaining_steps]) do
    ApathyDrive.Repo.delete!(mobile)

    kill(mobile, remaining_steps)
  end
  def kill(_mobile, []) do
    Process.exit(self, :normal)
  end

end
