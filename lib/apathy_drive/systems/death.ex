defmodule Systems.Death do
  use Timex

  def kill(%Monster{spirit: %Spirit{} = spirit} = monster) do
    ApathyDrive.PubSub.broadcast!("rooms:#{monster.room_id}", {:monster_died, monster: monster, reward: monster.experience})

    spirit
    |> Map.put(:room_id, monster.room_id)
    |> Spirit.save

    spirit = Systems.Login.login(spirit.socket, spirit.socket_pid, spirit.id)

    spirit
    |> Spirit.send_scroll("<p>You leave the body of #{monster.name}.</p>")
    |> Systems.Prompt.update

    send(spirit.socket_pid, {:set_entity, spirit})

    MonsterTemplate.set_last_killed_at(monster)

    ApathyDrive.Repo.delete(monster)
    Process.exit(self, :normal)
  end

  def kill(%Monster{spirit: nil} = monster) do
    ApathyDrive.PubSub.broadcast!("rooms:#{monster.room_id}", {:monster_died, monster: monster, reward: monster.experience})

    MonsterTemplate.set_last_killed_at(monster)

    ApathyDrive.Repo.delete(monster)
    Process.exit(self, :normal)
  end

end
