defmodule Components.Player do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.Player, :value)
  end

  def value(entity, new_value) do
    ApathyDrive.Entity.notify(entity, {:set_player, new_value})
  end

  def serialize(_entity) do
    {"Player", nil}
  end

  def send_message(entity, message) do
    Players.send_message(value(entity), message)
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, value) do
    {:ok, value, value}
  end

  def handle_event({:set_player, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end

end
