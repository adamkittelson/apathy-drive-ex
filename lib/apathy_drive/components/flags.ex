defmodule Components.Flags do
  use Systems.Reload
  use GenEvent
  import Utility

  ### Public API
  def value(monster) do
    GenEvent.call(monster, Components.Flags, :value)
  end

  def add_flag(monster, flag, amount_to_add) do
    GenEvent.notify(monster, {:add_flag, flag, amount_to_add})
  end

  def set_flag(monster, flag, value) do
    GenEvent.notify(monster, {:set_flag, flag, value})
  end

  def remove_flag(monster, flag) do
    GenEvent.notify(monster, {:remove_flag, flag})
  end

  def has_flag?(monster, flag) do
    value(monster)
    |> Map.has_key?(flag)
  end

  def serialize(entity) do
    %{"Flags" => value(entity)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, value) do
    {:ok, value, value}
  end

  def handle_event({:add_flag, flag, amount}, flags) do
    flags = Map.put_new(flags, flag, 0)
    {:ok, update_in(flags, [flag], &(&1 + amount))}
  end

  def handle_event({:set_flag, flag, value}, flags) do
    {:ok, Map.put(flags, flag, value)}
  end

  def handle_event({:remove_flag, flag}, flags) do
    {:ok, Map.delete(flags, flag)}
  end

  def handle_event(_, value) do
    {:ok, value}
  end
end
