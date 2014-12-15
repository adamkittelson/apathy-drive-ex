defmodule Systems.AI do
  use Systems.Reload

  def think(monster) do
    use_ability?(monster)
  end

  def use_ability?(monster) do
    heal(monster) || bless(monster)
  end

  def heal(monster) do
    max_hp  = Systems.HP.max(monster)
    current = Components.HP.value(monster)

    chance = trunc((max_hp - current) / max_hp * 100)
    :random.seed(:os.timestamp)

    roll = :random.uniform(100)

    if chance > roll do
      ability = monster
                |> Components.Abilities.heals
                |> random_ability
      if ability do
        ability.execute(monster)
      end
    end
  end

  def bless(monster) do
    ability = monster
              |> Components.Abilities.blessings
              |> random_ability

    if ability do
      ability.execute(monster)
    end
  end

  def random_ability(abilities) do
    case abilities do
      [ability] ->
        ability
      [] ->
        nil
      abilities ->
        :random.seed(:os.timestamp)
        abilities |> Enum.shuffle |> List.first
    end
  end

end
