defmodule Systems.Effect do
  use Timex
  alias ApathyDrive.{Mobile, TimerManager}

  def add(%{effects: _effects, last_effect_key: key} = entity, effect) do
    add_effect(entity, key + 1, effect)
  end

  def add(%{effects: _effects, last_effect_key: key} = entity, effect, duration) do
    key = key + 1
    entity =
      case entity do
        %{ref: ref} ->
          TimerManager.send_after(entity, {{:effect, key}, duration, {:remove_effect, ref, key}})
        %{} ->
          TimerManager.send_after(entity, {{:effect, key}, duration, {:remove_effect, key}})
      end

    effect = if effect && effect["timers"] do
      Map.put(effect, "timers", [{:effect, key} | effect["timers"]])
    else
      Map.put(effect, "timers", [{:effect, key}])
    end

    add_effect(entity, key, effect)
  end

  def add_effect(%{effects: effects, last_effect_key: last_effect} = entity, key, %{"stack_key" => stack_key, "stack_count" => stack_count} = effect) do
    case stack_count(entity, stack_key) do
      count when count < stack_count ->
        effects = Map.put(effects, key, effect)
        entity
        |> Map.put(:effects, effects)
        |> Map.put(:last_effect_key, last_effect + 1)
      _count ->
        entity
        |> remove_oldest_stack(effect["stack_key"])
        |> add_effect(key, effect)
    end
  end

  def add_effect(%{effects: effects, last_effect_key: last_effect} = entity, key, effect) do
    if Map.has_key?(effect, "application_message") do
      Mobile.send_scroll(entity, "<p><span class='dark-yellow'>#{effect["application_message"]}</span></p>")
    end

    if Map.has_key?(effect, "member") do
      ApathyDrive.PubSub.subscribe(effect["member"])
    end

    effects = Map.put(effects, key, effect)
    entity
    |> Map.put(:effects, effects)
    |> Map.put(:last_effect_key, last_effect + 1)
  end

  def remove_oldest_stack(%{effects: _effects} = entity, stack_key) do
    oldest = entity
             |> stack(stack_key)
             |> Enum.sort
             |> List.first

    if stack_key == :cast_timer do
      Mobile.send_scroll(entity, "<p><span class='dark-red'>You interrupt your other ability.</span></p>")
    end
    remove(entity, oldest)
  end

  def remove_all(%{effects: effects} = entity) do
    effects
    |> Map.keys
    |> Enum.reduce(entity, fn(key, entity) ->
         remove(entity, key)
       end)
  end

  def remove(%{effects: effects} = entity, key, opts \\ []) do
    case effects[key] do
      %{} ->
        if opts[:fire_after_cast] && Map.has_key?(effects[key], "after_cast") do
          #ApathyDrive.Spell.after_cast(effects[key]["after_cast"], [entity.ref])
        end

        if opts[:show_expiration_message] && Map.has_key?(effects[key], "RemoveMessage") do
          Mobile.send_scroll(entity, "<p><span class='dark-yellow'>#{effects[key]["RemoveMessage"]}</span></p>")
        end

        if Map.has_key?(effects[key], "member") do
          ApathyDrive.PubSub.unsubscribe(effects[key]["member"])
        end

        if Map.has_key?(effects[key], "timers") do
          Enum.each(effects[key]["timers"], fn(timer_name) ->
            TimerManager.cancel(entity, timer_name)
          end)
        end

        #if Map.has_key?(entity, :ref), do: send(self, {:think, entity.ref})

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

  def max_stacks?(%{} = monster, %{"duration_effects" => %{"stack_key" => stack_key, "stack_count" => stack_count}}) do
    stack_count(monster, stack_key) >= stack_count
  end
  def max_stacks?(%{} = monster, %{"duration_effects" => _} = ability) do
    ability = put_in(ability["duration_effects"]["stack_key"],   ability["name"])
    ability = put_in(ability["duration_effects"]["stack_count"], 1)
    max_stacks?(monster, ability)
  end
  def max_stacks?(%{}, %{}), do: false

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

  def effect_bonus(%{effects: effects}, name) do
    effects
    |> Map.values
    |> Enum.map(fn
         (%{} = effect) ->
           key =
             effect
             |> Map.keys
             |> Enum.find(fn key ->
                  String.downcase(to_string(key)) == String.downcase(to_string(name))
                end)

           if key, do: Map.get(effect, key, 0), else: 0
         (_) ->
           0
       end)
    |> Enum.sum
  end

  def find_by_ref(%{effects: effects}, ref) do
    Enum.find(effects, fn {_key, effect} ->
      effect["effect_ref"] == ref
    end)
  end

  def time_to_next_periodic_effect(%{effects: effects} = _mobile) do
    effects
    |> Map.values
    |> Enum.sort_by(& &1["NextEffectAt"])
    |> List.first
    |> case do
         %{"NextEffectAt" => time, "effect_ref" => ref} ->
           %{time: time - System.monotonic_time(:milliseconds), ref: ref}
         _ ->
           nil
       end
  end

  def schedule_next_periodic_effect(%{ref: ref} = mobile) do
    case time_to_next_periodic_effect(mobile) do
      %{ref: effect_ref, time: time} ->
        TimerManager.send_after(mobile, {:periodic_effects, time, {:periodic_effects, ref, effect_ref}})
      nil ->
        mobile
    end
  end

end
