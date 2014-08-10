defmodule Components.Effects do
  use Systems.Reload
  use GenEvent

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Effects, :value)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_effects, new_value})
  end

  def add(entity, key, effect) do
    IO.puts "Adding effect: #{inspect effect}"
    GenEvent.notify(entity, {:add_effect, key, effect})
  end

  def remove(entity, key) do
    GenEvent.notify(entity, {:remove_effect, key})
    IO.puts "Removed effect: #{inspect key}"
  end

  def serialize(_entity) do
    %{"Effects" => %{}}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, value) do
    {:ok, value, value}
  end

  def handle_event({:add_effect, key, effect}, value) do
    {:ok, Map.put(value, key, effect)}
  end

  def handle_event({:remove_effect, key}, value) do
    {:ok, Map.delete(value, key)}
  end

  def handle_event(_, value) do
    {:ok, value}
  end
end
