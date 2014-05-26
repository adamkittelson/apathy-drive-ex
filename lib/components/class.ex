defmodule Components.Class do
  use GenEvent.Behaviour

  ### Public API
  def value(entity) do
    :gen_event.call(entity, Components.Class, :value)
  end

  def value(entity, new_value) do
    Entity.notify(entity, {:set_class, new_value})
  end

  def serialize(entity) do
    %{"Class" => Components.Number.get_number(value(entity))}
  end

  ### GenEvent API
  def init(value) when is_number(value) do
    class = Classes.find_by_number(value)
    {:ok, class}
  end

  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, value) do
    {:ok, value, value}
  end

  def handle_event({:set_class, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
