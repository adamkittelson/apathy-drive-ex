defmodule Components.Name do
  use GenEvent.Behaviour

  ### Public API
  def get_name(entity) do
    :gen_event.call(entity, Components.Name, :get_name)
  end

  def serialize(entity) do
    {"Name", get_name(entity)}
  end

  ### GenEvent API
  def init(name) do
    {:ok, name}
  end

  def handle_call(:get_name, name) do
    {:ok, name, name}
  end
end
