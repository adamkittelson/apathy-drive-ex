defmodule Systems.Effect do
  import BlockTimer
  use Timex

  def add(%{effects: _effects} = entity, effect) do
    key = Time.now(:msecs) * 1000 |> trunc

    add_effect(entity, key, effect)
  end

  def add(%{effects: _effects} = entity, effect, duration) do
    key = Time.now(:msecs) * 1000 |> trunc

    TimerManager.call_after(entity, {{:effect, key}, duration |> seconds, fn ->
      remove(entity, key)
    end})

    effect = if effect && effect[:timers] do
      Map.put(effect, :timers, [{:effect, key} | effect[:timers]])
    else
      Map.put(effect, :timers, [{:effect, key}])
    end

    add_effect(entity, key, effect)
  end

  def add_effect(%{effects: _effects} = entity, key, %{stack_key: stack_key, stack_count: stack_count} = effect) do
    case stack_count(entity, stack_key) do
      count when count < stack_count ->
        put_in entity, [:timers, key], effect
      count ->
        entity
        |> remove_oldest_stack(effect[:stack_key])
        |> add_effect(key, effect)
    end
  end

  def handle_call(%{effects: _effects} = entity, key, effect) do
    put_in entity, [:effects, key], effect
  end

  def remove_oldest_stack(%{effects: effects} = entity, stack_key) do
    oldest = effects
             |> stack(stack_key)
             |> Enum.sort
             |> List.first

    if stack_key == :cast_timer do
      send_scroll(entity, "<p><span class='dark-red'>You interrupt your other ability.</span></p>")
    end
    remove(entity, oldest)
  end

  def remove(%{effects: effects} = entity, key) do
    case effects[key] do
      %{:timers => timers, :wear_message => wear_message} ->
        send_scroll(entity, "<p><span class='dark-cyan'>#{wear_message}</span></p>")

        Enum.reduce(timers, entity, fn(timer_name, entity) ->
          TimerManager.cancel(entity, timer_name)
        end)
      %{:timers => timers} ->
        Enum.reduce(timers, entity, fn(timer_name, entity) ->
          TimerManager.cancel(entity, timer_name)
        end)
      _ ->
        entity
    end
  end

  def stack_count(%{effects: _effects} = entity, stack_key) do
    stack(entity, stack_key)
    |> length
  end

  def stack(%{effects: effects}, stack_key) do
    effects
    |> Map.keys
    |> Enum.filter(fn(key) ->
         effects[key][:stack_key] == stack_key
       end)
  end

  def send_scroll(%Monster{} = monster, message) do
    Monster.send_scroll(monster, message)
  end

  def send_scroll(%Spirit{} = spirit, message) do
    Spirit.send_scroll(spirit, message)
  end

  def send_scroll(_, _), do: nil

end
