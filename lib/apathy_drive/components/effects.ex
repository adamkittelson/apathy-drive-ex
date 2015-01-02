defmodule Components.Effects do
  use Systems.Reload
  use GenEvent
  import Utility

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Effects, :value)
  end

  def add(entity, key, effect) do
    case GenEvent.call(entity, __MODULE__, {:add_effect, key, effect}) do
      :ok -> :ok
      :max_stacks ->
        remove_oldest_stack(entity, effect[:stack_key])
        add(entity, key, effect)
    end
  end

  def remove(entity, key) do
    case GenEvent.call(entity, __MODULE__, {:remove_effect, key}) do
      nil ->
        nil
      message ->
        send_message(entity, "scroll", "<p><span class='dark-cyan'>#{message}</span></p>")
    end
  end

  def lights(entity) do
    entity
    |> value
    |> Map.values
    |> Enum.filter(fn(effect) ->
         Map.has_key?(effect, :light)
       end)
    |> Enum.map(fn(effect) ->
         Map.get(effect, :light)
       end)
  end

  def damage_increases(entity) do
    entity
    |> value
    |> Map.values
    |> Enum.filter(fn(effect) ->
         Map.has_key?(effect, :damage_increase)
       end)
    |> Enum.map(fn(effect) ->
         Map.get(effect, :damage_increase)
       end)
  end

  def update(entity, fun) do
    GenEvent.call(entity, __MODULE__, {:update_effect, fun})
  end

  def remove_oldest_stack(entity, stack_key) do
    oldest = entity
      |> value
      |> stack(stack_key)
      |> Enum.sort
      |> List.first

    if stack_key == :cast_timer do
      send_message(entity, "scroll", "<p><span class='dark-red'>You interrupt your other ability.</span></p>")
    end
    remove(entity, oldest)
  end

  def remove(entity) do
    value(entity)
    |> Map.keys
    |> Enum.each &remove(entity, &1)
  end

  def max_stacks?(entity, %{stack_key: stack_key, stack_count: stack_count}) do
    stack_count(value(entity), stack_key) >= stack_count
  end
  def max_stacks?(entity, _), do: false

  defp stack_count(value, stack_key) do
    stack(value, stack_key)
    |> length
  end

  defp stack(value, stack_key) do
    value
    |> Map.keys
    |> Enum.filter(fn(key) ->
         value[key][:stack_key] == stack_key
       end)
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

  def handle_call({:add_effect, key, %{stack_key: stack_key, stack_count: stack_count} = effect}, value) do
    case stack_count(value, stack_key) do
      count when count < stack_count ->
        {:ok, :ok, Map.put(value, key, effect)}
      count ->
        {:ok, :max_stacks, value}
    end
  end

  def handle_call({:add_effect, key, effect}, value) do
    {:ok, :ok, Map.put(value, key, effect)}
  end

  def handle_call({:update_effect, fun}, value) do
    {:ok, :ok, fun.(value)}
  end

  def handle_call({:remove_effect, key}, value) do
    case value[key] do
      %{:timers => timers} ->
        Enum.each(timers, fn(timer) ->
          :erlang.cancel_timer(timer)
        end)
      _ ->
    end

    {:ok, value[key][:wear_message], Map.delete(value, key)}
  end

  def handle_event(_, value) do
    {:ok, value}
  end
end
