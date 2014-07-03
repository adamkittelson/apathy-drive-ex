defmodule Components.Trainer do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Trainer, :value)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_trainer, new_value})
  end

  def skills(entity) do
    value(entity)
    |> Enum.map &(Skills.value[&1])
  end

  def serialize(entity) do
    %{"Trainer" => value(entity)}
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
