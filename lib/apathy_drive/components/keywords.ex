defmodule Components.Keywords do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Keywords, :value)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_keywords, new_value})
  end

  def serialize(entity) do
    %{"Keywords" => value(entity)}
  end

  ### GenEvent API
  def init(exit_ids) do
    {:ok, exit_ids}
  end

  def handle_call(:value, exit_ids) do
    {:ok, exit_ids, exit_ids}
  end

  def handle_event({:set_keywords, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
