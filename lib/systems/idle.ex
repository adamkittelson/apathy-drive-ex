defmodule Systems.Idle do
  use Systems.Reload

  def initialize do
    :timer.apply_interval(1_000, Systems.Idle, :increment, [])
  end

  def increment do
    Components.all(Components.Idle)
    |> Enum.filter(&(Components.Online.value(&1)))
    |> Enum.each(&(Components.Idle.add(&1, 1)))
  end

  def idle?(entity) do
    Components.Idle.value(entity) >= 60
  end

end
