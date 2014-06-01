defmodule Components.Skills do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.Skills, :value)
  end

  def value(entity, new_value) do
    Entity.notify(entity, {:set_skills, new_value})
  end

  def serialize(entity) do
    %{"Skills" => value(entity)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, value) do
    {:ok, value, value}
  end

  def handle_event(_, value) do
    {:ok, value}
  end
end
