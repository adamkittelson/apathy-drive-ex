defmodule Systems.Death do
  use Timex

  def kill(%Monster{spirit: %Spirit{} = spirit} = monster) do
    ApathyDrive.PubSub.broadcast!("rooms:#{monster.room_id}", {:monster_died, monster: monster, reward: monster.experience})

    spirit
    |> Map.put(:room_id, monster.room_id)
    |> Spirit.save

    spirit = Systems.Login.login(spirit.socket, spirit.id)

    spirit
    |> Spirit.send_scroll("<p>You leave the body of #{monster.name}.</p>")
    |> Systems.Prompt.update

    ApathyDrive.Repo.delete(monster)
    Process.exit(self, :normal)
  end

  def kill(%Monster{spirit: nil} = monster) do
    ApathyDrive.PubSub.broadcast!("rooms:#{monster.room_id}", {:monster_died, monster: monster, reward: monster.experience})

    ApathyDrive.Repo.delete(monster)
    Process.exit(self, :normal)
  end

end
