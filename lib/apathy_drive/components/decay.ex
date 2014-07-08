defmodule Components.Decay do
  use Systems.Reload
  use GenEvent
  use Timex

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Decay, :value)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_decay, new_value})
  end

  def state(entity) do
    value(entity)["state"]
  end

  def frequency(entity) do
    value(entity)["frequency"]
  end

  def decay_at(entity) do
    value(entity)["decay_at"]
  end

  def set_decay_at(entity) do
    GenEvent.notify(entity, :set_decay_at)
  end

  def decay(entity) do
    entity
    |> Components.Name.value(entity
                             |> Components.Name.value
                             |> String.replace("the corpse", "the decayed corpse"))
    GenEvent.notify(entity, :decay)
  end

  def serialize(entity) do
    %{"Decay" => put_in(value(entity)["decay_at"], Date.convert(value(entity)["decay_at"], :secs))}
  end

  ### GenEvent API
  def init(value) do
    {:ok, put_in(value["decay_at"], Date.from(value["decay_at"], :secs))}
  end

  def handle_call(:value, lair) do
    {:ok, lair, lair}
  end

  def handle_event({:set_lair, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(:set_decay_at, value) do
    value = put_in(value["decay_at"], Date.shift(value["decay_at"], mins: value["frequency"]))
    {:ok, value}
  end

  def handle_event(:decay, value) do
    {:ok, put_in(value["state"], "decayed")}
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
