defmodule ApathyDrive.Aggression do
  alias ApathyDrive.Mobile

  def react(%{mobile: %Mobile{} = mobile, unity: unity}, %{unity: intruder_unity}) when not is_nil(unity) and unity == intruder_unity do
    mobile
  end

  def react(%{mobile: %Mobile{} = mobile, alignment: "good"}, %{intruder: intruder, alignment: "evil"}) do
    attack(mobile, intruder)
  end

  def react(%{mobile: %Mobile{} = mobile, alignment: "good"}, _) do
    mobile
  end

  def react(%{mobile: %Mobile{} = mobile, alignment: "neutral"}, _) do
    mobile
  end

  def react(%{mobile: %Mobile{} = mobile, alignment: "evil", unity: unity}, %{intruder: _intruder, alignment: "evil", unity: intruder_unity}) when unity == intruder_unity do
    mobile
  end

  def react(%{mobile: %Mobile{} = mobile, alignment: "evil", unity: _unity}, %{intruder: intruder, alignment: _alignment, unity: _intruder_unity}) do
    attack(mobile, intruder)
  end

  def attack(%Mobile{} = mobile, intruder) do
    put_in(mobile.hate, Map.update(mobile.hate, intruder, 1, fn(hate) -> hate + 1 end))
  end

end
