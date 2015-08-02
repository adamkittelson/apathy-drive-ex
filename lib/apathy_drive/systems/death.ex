defmodule Systems.Death do
  import Systems.Text

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

  def kill(%Monster{spirit: nil} = monster) do
    ApathyDrive.PubSub.broadcast!("rooms:#{monster.room_id}", {:monster_died, monster: monster, reward: monster.experience})
    ApathyDrive.Factions.add_to_bonus_pool(monster.experience)

    MonsterTemplate.set_last_killed_at(monster)

    ApathyDrive.Repo.delete!(monster)
    Process.exit(self, :normal)
  end

end
