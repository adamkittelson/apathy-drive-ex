defmodule Systems.Death do
  import Systems.Text
  alias ApathyDrive.Mobile

  def kill(%Monster{spirit: %Spirit{} = spirit} = monster) do
    ApathyDrive.PubSub.broadcast!("rooms:#{monster.room_id}", {:monster_died, monster: monster, reward: monster.experience})
    ApathyDrive.Factions.add_to_bonus_pool(monster.experience)
    MonsterTemplate.set_last_killed_at(monster)
    ApathyDrive.Repo.delete!(monster)

    death_message =
      monster.death_message
      |> interpolate(%{"name" => monster.name})
      |> capitalize_first

    :global.unregister_name(:"spirit_#{spirit.id}")

    spirit =
      spirit
      |> Map.put(:room_id, monster.room_id)
      |> Map.put(:mana, min(monster.mana, spirit.max_mana))
      |> Spirit.login
      |> Spirit.send_scroll("<p>You leave the body of #{monster.name}.</p>")
      |> Spirit.send_scroll("<p>#{death_message}</p>")
      |> Systems.Prompt.update
      |> Spirit.save

    send(spirit.socket_pid, {:set_entity, spirit})
    Process.exit(self, :normal)
  end

  # Player not possessing a monster
  def kill(%Mobile{monster_template_id: nil, spirit: %Spirit{} = spirit} = mobile) do
    kill(mobile, [:inform_player, :respawn_spirit])
  end
  # Player possessing a monster
  def kill(%Mobile{monster_template_id: _, spirit: %Spirit{} = spirit} = mobile) do
    kill(mobile, [:reward_monster_death_exp, :respawn_spirit, :unpossess, :set_last_killed_at])
  end
  # Monster not possessed by a player
  def kill(%Mobile{spirit: nil} = mobile) do
    kill(mobile, [:reward_monster_death_exp, :generate_loot, :set_last_killed_at])
  end

  def kill(mobile, [:reward_monster_death_exp | remaining_steps]) do
    ApathyDrive.PubSub.broadcast!("rooms:#{mobile.room_id}:mobiles", {:mobile_died, mobile: mobile, reward: mobile.experience})

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
    mobile.room_id
    |> Room.find
    |> send({:generate_loot, mobile.level})

    kill(mobile, remaining_steps)
  end
  def kill(mobile, [:set_last_killed_at | remaining_steps]) do
    MonsterTemplate.set_last_killed_at(mobile)

    kill(mobile, remaining_steps)
  end
  def kill(mobile, []) do
    Process.exit(self, :normal)
  end

end
