defmodule ApathyDrive.Aggression do
  alias ApathyDrive.Mobile

  def react(%{mobile: %Mobile{} = mobile}, %{invisible?: true}), do: mobile

  def react(%{mobile: %Mobile{} = mobile, unities: unities}, %{unities: intruder_unities}) when length(unities) > 0 and unities == intruder_unities do
    mobile
  end

  def react(%{mobile: %Mobile{} = mobile, alignment: "good"}, %{mobile: intruder, alignment: "evil"}) do
    attack(mobile, intruder)
  end

  def react(%{mobile: %Mobile{} = mobile, alignment: "good"}, _) do
    mobile
  end

  def react(%{mobile: %Mobile{} = mobile, alignment: "neutral"}, _) do
    mobile
  end

  def react(%{mobile: %Mobile{} = mobile, alignment: "evil", unities: unities}, %{mobile: _intruder, alignment: "evil", unities: intruder_unities}) when length(unities) > 0 and unities == intruder_unities do
    mobile
  end

  def react(%{mobile: %Mobile{} = mobile, alignment: "evil", spawned_at: spawned_at}, %{mobile: _intruder, alignment: "evil", spawned_at: intruder_spawned_at}) when spawned_at == intruder_spawned_at do
    mobile
  end

  # evil monsters will attack each other if their names aren't similar enough, superficial bastards
  def react(%{mobile: %Mobile{} = mobile, alignment: "evil", name: name}, %{mobile: intruder, alignment: "evil", name: intruder_name}) do
    if String.jaro_distance(name, intruder_name) < 0.65 do
      attack(mobile, intruder)
    else
      mobile
    end
  end

  def react(%{mobile: %Mobile{} = mobile, alignment: "evil"}, %{mobile: intruder, alignment: _alignment}) do
    attack(mobile, intruder)
  end

  def attack(%Mobile{} = mobile, intruder) do
    put_in(mobile.hate, Map.update(mobile.hate, intruder, 1, fn(hate) -> hate + 1 end))
  end

end
