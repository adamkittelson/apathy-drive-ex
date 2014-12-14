defmodule Systems.AI do
  use Systems.Reload

  def think(monster) do
    if Components.Combat.in_combat?(monster) do
      use_ability?(monster)
    end
  end

  def use_ability?(monster) do
    heal(monster)
  end

  def heal(monster) do
    max_hp  = Systems.HP.max(monster)
    current = Components.HP.value(monster)

    chance = trunc((max_hp - current) / max_hp * 100)
    :random.seed(:os.timestamp)

    roll = :random.uniform(100)

    if chance > roll do
      ability = case Components.Abilities.heals(monster) do
        [heal] ->
          heal
        [] ->
          nil
        heals ->
          heals |> Enum.shuffle |> List.first
      end

      if ability do
        Components.Module.value(ability).execute(monster, "")
      end
    end
  end

end
