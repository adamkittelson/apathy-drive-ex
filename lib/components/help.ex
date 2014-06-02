defmodule Components.Help do
  use Systems.Reload
  use GenEvent.Behaviour

  ### Public API
  def get_help(entity) do
    :gen_event.call(entity, Components.Help, :get_help)
  end

  def get_keywords(entity) do
    :gen_event.call(entity, Components.Help, :get_keywords)
  end

  def get_name(entity) do
    :gen_event.call(entity, Components.Help, :get_name)
  end

  def value(entity) do
    :gen_event.call(entity, Components.Help, :value)
  end

  def value(entity, new_value) do
    Entity.notify(entity, {:set_help, new_value})
  end

  def serialize(entity) do
    %{"Help" => value(entity)}
  end

  ### GenEvent API
  def init(state) do
    {:ok, state}
  end

  def handle_call(:value, state) do
    {:ok, state, state}
  end

  def handle_call(:get_help, state) do
    {:ok, state["help"], state}
  end

  def handle_call(:get_keywords, state) do
    {:ok, state["keywords"], state}
  end

  def handle_call(:get_name, state) do
    {:ok, state["name"], state}
  end

  def handle_event({:set_help, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end

end
