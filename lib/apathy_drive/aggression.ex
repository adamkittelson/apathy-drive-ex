defmodule ApathyDrive.Aggression do
  alias ApathyDrive.Monster

  # Don't attack spirits without bodies
  def react(%Monster{} = monster, %Monster{monster_template_id: nil}), do: monster

  # monsters with unities attack monsters with different unities
  def react(%Monster{unities: unities} = monster, %Monster{unities: intruder_unities} = intruder) when length(unities) > 0 and unities != intruder_unities do
    attack(monster, intruder)
  end

  def react(%Monster{unities: [], alignment: "good"} = monster, %Monster{alignment: "evil"} = intruder) do
    attack(monster, intruder)
  end

  def react(%Monster{unities: [], alignment: "good"} = monster, _) do
    monster
  end

  def react(%Monster{unities: [], alignment: "neutral"} = monster, _) do
    monster
  end

  def react(%Monster{unities: [], alignment: "evil", area_spawned_in: monster_area} = monster, %Monster{area_spawned_in: intruder_area, alignment: intruder_alignment} = intruder) when monster_area != intruder_area or intruder_alignment != "evil" do
    attack(monster, intruder)
  end

  def react(%Monster{} = monster, _) do
    monster
  end

  def attack(%Monster{} = monster, %Monster{ref: ref}) do
    update_in(monster.hate[ref], &((&1 || 0) + 1))
  end

end
