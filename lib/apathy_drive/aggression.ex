defmodule ApathyDrive.Aggression do
  alias ApathyDrive.Mobile

  # Don't attack spirits without bodies
  def react(%Mobile{} = mobile, %Mobile{monster_template_id: nil}), do: mobile

  # mobiles with unities attack mobiles with different unities
  def react(%Mobile{unities: unities} = mobile, %Mobile{unities: intruder_unities} = intruder) when length(unities) > 0 and unities != intruder_unities do
    attack(mobile, intruder)
  end

  def react(%Mobile{unities: [], alignment: "good"} = mobile, %Mobile{alignment: "evil"} = intruder) do
    attack(mobile, intruder)
  end

  def react(%Mobile{unities: [], alignment: "good"} = mobile, _) do
    mobile
  end

  def react(%Mobile{unities: [], alignment: "neutral"} = mobile, _) do
    mobile
  end

  def react(%Mobile{unities: [], alignment: "evil", area_spawned_in: mobile_area} = mobile, %Mobile{area_spawned_in: intruder_area, alignment: intruder_alignment} = intruder) when mobile_area != intruder_area or intruder_alignment != "evil" do
    attack(mobile, intruder)
  end

  def react(%Mobile{} = mobile, _) do
    mobile
  end

  def attack(%Mobile{} = mobile, %Mobile{ref: ref}) do
    update_in(mobile.hate[ref], &((&1 || 0) + 1))
  end

end
