defmodule Systems.Aggression do

  def react(%{monster: %Monster{touched?: true} = monster, alignment: "good"}, %{intruder: _intruder, alignment: "good", lair_id: _lair}) do
    monster
  end

  def react(%{monster: %Monster{touched?: true} = monster, alignment: "good"}, %{intruder: intruder, alignment: _, lair_id: _}) do
    attack(monster, intruder)
  end

  def react(%{monster: %Monster{touched?: false} = monster, alignment: "good"}, %{intruder: intruder, alignment: "evil", lair_id: _lair}) do
    attack(monster, intruder)
  end

  def react(%{monster: %Monster{touched?: false} = monster, alignment: "good"}, _) do
    monster
  end

  def react(%{monster: %Monster{touched?: true} = monster, alignment: "neutral"}, %{intruder: _intruder, alignment: "neutral", lair_id: _lair}) do
    monster
  end

  def react(%{monster: %Monster{touched?: true} = monster, alignment: "neutral"}, %{intruder: intruder, alignment: _, lair_id: _lair}) do
    attack(monster, intruder)
  end

  def react(%{monster: %Monster{touched?: false} = monster, alignment: "neutral"}, _) do
    monster
  end

  def react(%{monster: %Monster{touched?: true} = monster, alignment: "evil"}, %{intruder: _intruder, alignment: "evil", lair_id: _lair}) do
    monster
  end

  def react(%{monster: %Monster{touched?: true} = monster, alignment: "evil"}, %{intruder: intruder, alignment: _, lair_id: _lair}) do
    attack(monster, intruder)
  end

  def react(%{monster: %Monster{touched?: false, lair_id: lair_id} = monster, alignment: "evil"}, %{intruder: _intruder, alignment: "evil", lair_id: lair}) when lair_id == lair do
    monster
  end

  def react(%{monster: %Monster{touched?: false} = monster, alignment: "evil"}, %{intruder: intruder, alignment: _alignment, lair_id: _lair}) do
    attack(monster, intruder)
  end

  def attack(%Monster{} = monster, intruder) do
    put_in(monster.hate, HashDict.update(monster.hate, intruder, 1, fn(hate) -> hate + 1 end))
  end

end