defmodule Systems.Idle do
  use Systems.Reload
  import BlockTimer

  def initialize do
    apply_interval 1 |> seconds, do: increment
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
