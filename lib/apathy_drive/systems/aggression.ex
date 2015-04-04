defmodule Systems.Aggression do

  def react(%{monster: %Monster{disposition: "lawful"} = monster, alignment: "good"}, %{intruder: intruder, alignment: "evil"}) do
    attack(monster, intruder)
  end

  def react(%{monster: %Monster{disposition: "chaotic"} = monster, alignment: "good"}, %{intruder: intruder, alignment: "evil"}) do
    if :random.uniform(10) == 10 do
      attack(monster, intruder)
    else
      monster
    end
  end

  def react(%{monster: %Monster{disposition: "chaotic"} = monster, alignment: "neutral"}, %{intruder: intruder, alignment: "good"}) do
    if :random.uniform(10) == 10 do
      attack(monster, intruder)
    else
      monster
    end
  end

  def react(%{monster: %Monster{disposition: "chaotic"} = monster, alignment: "neutral"}, %{intruder: intruder, alignment: "evil"}) do
    if :random.uniform(10) == 10 do
      attack(monster, intruder)
    else
      monster
    end
  end

  def react(%{monster: %Monster{disposition: "lawful"} = monster, alignment: "evil"}, %{intruder: intruder, alignment: "good"}) do
    attack(monster, intruder)
  end

  def react(%{monster: %Monster{disposition: "lawful"} = monster, alignment: "evil"}, %{intruder: intruder, alignment: "neutral"}) do
    attack(monster, intruder)
  end

  def react(%{monster: %Monster{disposition: "chaotic"} = monster, alignment: "evil"}, %{intruder: intruder, alignment: "good"}) do
    attack(monster, intruder)
  end

  def react(%{monster: %Monster{disposition: "chaotic"} = monster, alignment: "evil"}, %{intruder: intruder, alignment: "neutral"}) do
    attack(monster, intruder)
  end

  def react(%{monster: %Monster{} = monster}, _), do: monster

  def attack(%Monster{} = monster, intruder) do
    put_in(monster.hate, HashDict.update(monster.hate, intruder, 1, fn(hate) -> hate + 1 end))
  end

end