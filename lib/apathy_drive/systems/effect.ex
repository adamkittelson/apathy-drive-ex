defmodule Systems.Effect do
  import BlockTimer
  use Timex

  def add(%{effects: _effects} = entity, effect) do
    key = Time.now(:msecs) * 1000 |> trunc

    add_effect(entity, key, effect)
  end

  def add(%{effects: _effects} = entity, effect, duration) do
    key = Time.now(:msecs) * 1000 |> trunc

    entity = TimerManager.call_after(entity, {{:effect, key}, duration |> seconds, fn ->
      send(self, {:remove_effect, key})
    end})

    effect = if effect && effect["timers"] do
      Map.put(effect, "timers", [{:effect, key} | effect["timers"]])
    else
      Map.put(effect, "timers", [{:effect, key}])
    end

    add_effect(entity, key, effect)
  end

  def add_effect(%{effects: effects} = entity, key, %{"stack_key" => stack_key, "stack_count" => stack_count} = effect) do
    case stack_count(entity, stack_key) do
      count when count < stack_count ->
        effects = Map.put(effects, key, effect)
        Map.put(entity, :effects, effects)
      _count ->
        entity
        |> remove_oldest_stack(effect["stack_key"])
        |> add_effect(key, effect)
    end
  end

  def add_effect(%{effects: effects} = entity, key, effect) do
    effects = Map.put(effects, key, effect)
    Map.put(entity, :effects, effects)
  end

  def remove_oldest_stack(%{effects: _effects} = entity, stack_key) do
    oldest = entity
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
      %{"timers" => timers, "expiration_message" => expiration_message} ->
        send_scroll(entity, "<p><span class='dark-yellow'>#{expiration_message}</span></p>")

        Enum.each(timers, fn(timer_name) ->
          TimerManager.cancel(entity, timer_name)
        end)
        send(self, :think)
        Map.put entity, :effects, Map.delete(effects, key)
      %{"timers" => timers} ->
        Enum.each(timers, fn(timer_name) ->
          TimerManager.cancel(entity, timer_name)
        end)
        send(self, :think)
        Map.put entity, :effects, Map.delete(effects, key)
      _ ->
        found_key = effects
                    |> Map.keys
                    |> Enum.find(fn(existing_key) ->
                         effects[existing_key]["timers"] == [key]
                       end)

        if found_key do
          remove(entity, found_key)
        else
          entity
        end
    end
  end

  def max_stacks?(%Monster{} = monster, %Ability{properties: %{"duration_effects" => %{"stack_key" => stack_key, "stack_count" => stack_count}}}) do
    stack_count(monster, stack_key) >= stack_count
  end

  def max_stacks?(%Monster{} = monster, %Ability{properties: %{"duration_effects" => _}} = ability) do
    ability = put_in(ability.properties["duration_effects"]["stack_key"],   ability.name)
    ability = put_in(ability.properties["duration_effects"]["stack_count"], 1)
    max_stacks?(monster, ability)
  end

  def max_stacks?(%Monster{}, %Ability{}), do: false

  def stack_count(%{effects: _effects} = entity, stack_key) do
    stack(entity, stack_key)
    |> length
  end

  def stack(%{effects: effects}, stack_key) do
    effects
    |> Map.keys
    |> Enum.filter(fn(key) ->
         effects[key]["stack_key"] == stack_key
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
