defmodule Systems.Level do

  def exp_to_next_level(entity) do
    exp_at_level(entity.level + 1) - entity.experience
  end

  def exp_for_level(1), do: 0
  def exp_for_level(lvl) do
    ((1 + :math.pow(lvl - 2, 1.50 + (lvl / 75.0))) * 100000 / 150)
    |> trunc
  end

  def exp_at_level(0), do: nil
  def exp_at_level(level) do
    (1..level)
    |> Enum.reduce(0, fn(lvl, total) ->
         total + exp_for_level(lvl)
       end)
  end

  def advance(monster_or_spirit) do
    advance(monster_or_spirit, exp_to_next_level(monster_or_spirit))
  end

  def advance(%Monster{} = monster, exp_tnl) when exp_tnl < 1 do
    monster = put_in(monster.level, monster.level + 1)

    Monster.send_scroll(monster, "<p>Your #{monster.name} has advanced to level #{monster.level}!</p>")
  end
  def advance(%Spirit{} = spirit, exp_tnl) when exp_tnl < 1 do
    spirit = put_in(spirit.level, spirit.level + 1)

    Spirit.send_scroll(spirit, "<p>You've advanced to level #{spirit.level}!</p>")
  end
  def advance(spirit_or_monster, _exp_tnl), do: spirit_or_monster

end
