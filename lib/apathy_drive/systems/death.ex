defmodule Systems.Death do
  use Timex

  def kill(%Monster{} = monster) do
    ApathyDrive.PubSub.broadcast!("monsters:#{monster.id}",   {:possessed_monster_died, monster})
    ApathyDrive.PubSub.broadcast!("rooms:#{monster.room_id}", {:monster_died, monster: monster, reward: monster.experience})

    ApathyDrive.Repo.delete(monster)
    Process.exit(self, :normal)
  end

end
