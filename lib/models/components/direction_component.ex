defmodule ApathyDrive.DirectionComponent do
  use GenEvent.Behaviour

  ### Public API
  def get_direction(entity) do
    :gen_event.call(entity, ApathyDrive.DirectionComponent, :get_direction)
  end

  ### GenEvent API
  def init(direction) do
    {:ok, direction}
  end

  def handle_call(:get_direction, direction) do
    {:ok, direction, direction}
  end
end
