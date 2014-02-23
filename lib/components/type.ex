defmodule Components.Type do
  use GenEvent.Behaviour

  ### Public API
  def get_type(entity) do
    :gen_event.call(entity, Components.Type, :get_type)
  end
  
  def serialize(entity) do
    {"Type", get_type(entity)}
  end

  ### GenEvent API
  def init(type) do
    {:ok, type}
  end

  def handle_call(:get_type, type) do
    {:ok, type, type}
  end
end
