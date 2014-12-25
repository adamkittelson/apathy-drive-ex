defmodule Systems.Aggression do
  use Systems.Reload

  def monster_entered(intruder, room) do
    Systems.Monster.monsters_in_room(room, intruder)
    |> Enum.each(fn(monster) ->
         intruder = %{
           intruder:   intruder,
           alignment: Components.Alignment.get_alignment(intruder)
         }

         monster = %{
           monster:     monster,
           alignment:   Components.Alignment.get_alignment(monster),
           disposition: Components.Module.value(monster).disposition
         }

         react(monster, intruder)
       end)
  end

  def react(%{monster: monster, disposition: "lawful", alignment: "good"}, %{intruder: intruder, alignment: "evil"}) do
    attack(monster, intruder)
  end

  def react(%{monster: monster, disposition: "chaotic", alignment: "good"}, %{intruder: intruder, alignment: "evil"}) do
    :random.seed(:os.timestamp)

    if :random.uniform(10) == 10 do
      attack(monster, intruder)
    end
  end

  def react(%{monster: monster, disposition: "chaotic", alignment: "neutral"}, %{intruder: intruder, alignment: "good"}) do
    :random.seed(:os.timestamp)

    if :random.uniform(10) == 10 do
      attack(monster, intruder)
    end
  end

  def react(%{monster: monster, disposition: "chaotic", alignment: "neutral"}, %{intruder: intruder, alignment: "evil"}) do
    :random.seed(:os.timestamp)

    if :random.uniform(10) == 10 do
      attack(monster, intruder)
    end
  end

  def react(%{monster: monster, disposition: "lawful", alignment: "evil"}, %{intruder: intruder, alignment: "good"}) do
    attack(monster, intruder)
  end

  def react(%{monster: monster, disposition: "lawful", alignment: "evil"}, %{intruder: intruder, alignment: "neutral"}) do
    attack(monster, intruder)
  end

  def react(%{monster: monster, disposition: "chaotic", alignment: "evil"}, %{intruder: intruder, alignment: "good"}) do
    attack(monster, intruder)
  end

  def react(%{monster: monster, disposition: "chaotic", alignment: "evil"}, %{intruder: intruder, alignment: "neutral"}) do
    attack(monster, intruder)
  end

  def react(_, _), do: nil

  def attack(monster, intruder), do: Systems.Combat.enrage(monster, intruder)

end