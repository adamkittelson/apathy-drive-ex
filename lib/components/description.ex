defmodule Components.Description do
  use GenEvent.Behaviour

  ### Public API
  def get_description(entity) do
    :gen_event.call(entity, Components.Description, :get_description)
  end

  def serialize(entity) do
    {"Description", get_description(entity)}
  end

  ### GenEvent API
  def init(description) do
    {:ok, description}
  end

  def handle_call(:get_description, description) do
    {:ok, description, description}
  end
end
