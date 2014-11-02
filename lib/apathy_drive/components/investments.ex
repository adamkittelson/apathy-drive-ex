defmodule Components.Investments do
  use Systems.Reload
  use GenEvent
  import Utility

  ### Public API
  def value(spirit) do
    GenEvent.call(spirit, Components.Investments, :value)
  end

  def list(spirit) do
    GenEvent.call(spirit, Components.Investments, :list)
  end

  def power_invested(spirit, entity_id) do
    GenEvent.call(spirit, Components.Investments, {:power_invested, entity_id |> to_string})
  end

  def invest(spirit, entity_id, power) do
    GenEvent.notify(spirit, {:invest, entity_id |> to_string, power})
  end

  def uninvest(spirit, id) do
    GenEvent.notify(spirit, {:uninvest, id |> to_string})
  end

  def power_invested(spirit) do
    spirit
    |> cleanup
    |> list
    |> Enum.map(&power_invested(spirit, &1))
    |> Enum.sum
  end

  def cleanup(entity) do
    entity
    |> list
    |> Enum.reject(fn(id) ->
         id
         |> String.to_integer
         |> exists?
       end)
    |> Enum.each(fn(id) ->
         uninvest(entity, id)
       end)
    entity
  end

  def exists?(id) do
    !!(Characters.find_by_id(id) || Monsters.find_by_id(id))
  end

  def serialize(entity) do
    %{"Investments" => value(entity)}
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, value) do
    {:ok, value, value}
  end

  def handle_call(:list, value) do
    {:ok, Map.keys(value), value}
  end

  def handle_call({:power_invested, id}, value) do
    {:ok, value[id] || 0, value}
  end

  def handle_event({:invest, id, amount}, value) do
    value = Map.put_new(value, id, 0)
    current = value[id]
    new = put_in value[id], current + amount
    {:ok, new}
  end

  def handle_event({:uninvest, id}, value) do
    {:ok, Map.delete(value, id)}
  end

  def handle_event(_, value) do
    {:ok, value}
  end
end
