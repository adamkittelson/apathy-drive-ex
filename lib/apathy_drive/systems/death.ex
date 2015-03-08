defmodule Systems.Death do
  use Timex

  def kill(%Monster{} = monster) do
    room = Monster.find_room(monster)

    Phoenix.PubSub.broadcast!(ApathyDrive.PubSub, "monsters:#{monster.id}", {:possessed_monster_died, monster})
    Phoenix.PubSub.broadcast!(ApathyDrive.PubSub, "rooms:#{room.id}", {:monster_died, monster: monster, reward: experience_to_grant(monster)})

    ApathyDrive.Repo.delete(monster)
    Process.exit(self, :normal)
  end

  def experience_to_grant(%Monster{} = monster) do
    ["strength", "intelligence", "agility", "health"]
    |> Enum.reduce(0, fn(stat, total) ->
         total + Monster.pre_effect_bonus_stat(monster, stat)
       end)
    |> experience_to_grant
  end

  def experience_to_grant(stat_total) do
    trunc(stat_total * (1 + (stat_total * 0.005)))
  end

end
