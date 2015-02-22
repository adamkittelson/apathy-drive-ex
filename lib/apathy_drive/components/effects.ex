defmodule Components.Effects do
  use Systems.Reload
  use GenEvent
  import Utility

  # def lights(entity) do
  #   entity
  #   |> value
  #   |> Map.values
  #   |> Enum.filter(fn(effect) ->
  #        Map.has_key?(effect, :light)
  #      end)
  #   |> Enum.map(fn(effect) ->
  #        Map.get(effect, :light)
  #      end)
  # end
  # 
  # def damage_increases(entity) do
  #   entity
  #   |> value
  #   |> Map.values
  #   |> Enum.filter(fn(effect) ->
  #        Map.has_key?(effect, :damage_increase)
  #      end)
  #   |> Enum.map(fn(effect) ->
  #        Map.get(effect, :damage_increase)
  #      end)
  # end
  # 
  # def update(entity, fun) do
  #   GenEvent.call(entity, __MODULE__, {:update_effect, fun})
  # end
  # 
  # def remove(entity) do
  #   value(entity)
  #   |> Map.keys
  #   |> Enum.each &remove(entity, &1)
  # end
  # 
  # def max_stacks?(entity, %{stack_key: stack_key, stack_count: stack_count}) do
  #   stack_count(value(entity), stack_key) >= stack_count
  # end
  # def max_stacks?(entity, _), do: false
  # 
  # def serialize(_entity) do
  #   %{"Effects" => %{}}
  # end
  # 
  # ### GenEvent API
  # def init(value) do
  #   {:ok, value}
  # end
  # 
  # def handle_call(:value, value) do
  #   {:ok, value, value}
  # end
  # 
  # def handle_call({:update_effect, fun}, value) do
  #   {:ok, :ok, fun.(value)}
  # end
  # 
  # def handle_event(_, value) do
  #   {:ok, value}
  # end
end
