defmodule Systems.Idle do
  use Systems.Reload
  import BlockTimer

  def initialize do
    apply_interval 1 |> seconds, do: increment
  end

  def increment do
    Spirits.all
    |> Enum.each(&Spirit.increment_idle/1)
  end

  def idle?(spirit) do
    Spirit.idle(spirit) >= 60
  end

end
