defmodule Systems.Effect do
  use Timex
  alias ApathyDrive.{Mobile, TimerManager}
  import TimerManager, only: [seconds: 1]

  def add(%{effects: _effects, last_effect_key: key} = entity, effect) do
    add_effect(entity, key + 1, effect)
  end

  def add(%{effects: _effects, last_effect_key: key} = entity, effect, duration) do
    key = key + 1
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
        entity
        |> Map.put(:effects, effects)
        |> Map.put(:last_effect_key, key)
      _count ->
        entity
        |> remove_oldest_stack(effect["stack_key"])
        |> add_effect(key, effect)
    end
  end

  def add_effect(%{effects: effects} = entity, key, effect) do
    if Map.has_key?(effect, "application_message") do
      send_scroll(entity, "<p><span class='dark-yellow'>#{effect["application_message"]}</span></p>")
    end

    effects = Map.put(effects, key, effect)
    entity
    |> Map.put(:effects, effects)
    |> Map.put(:last_effect_key, key)
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
      %{"expiration_message" => expiration_message} ->
        send_scroll(entity, "<p><span class='dark-yellow'>#{expiration_message}</span></p>")

        Map.put entity, :effects, Map.delete(effects, key)
      %{} ->
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

  def max_stacks?(%Mobile{} = mobile, %{"duration_effects" => %{"stack_key" => stack_key, "stack_count" => stack_count}}) do
    stack_count(mobile, stack_key) >= stack_count
  end
  def max_stacks?(%Mobile{} = mobile, %{"duration_effects" => _} = ability) do
    ability = put_in(ability["duration_effects"]["stack_key"],   ability["name"])
    ability = put_in(ability["duration_effects"]["stack_count"], 1)
    max_stacks?(mobile, ability)
  end
  def max_stacks?(%Mobile{}, %{}), do: false

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

  def send_scroll(%Mobile{} = mobile, message) do
    Mobile.send_scroll(mobile, message)
  end

  def send_scroll(_, _), do: nil

end
