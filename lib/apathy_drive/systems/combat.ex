defmodule Systems.Combat do
  use Systems.Reload
  import Utility
  import Timer, except: [start: 0]
  use Timex

  def start(entity, time \\ 0.5) do
    {:ok, timer} = apply_after(time |> seconds, do: swing(entity))
    Components.Combat.set_timer entity, timer
  end

  def attack(entity, target) do
    Components.Hunting.add(entity, target)
    Components.Hunting.add(target, entity)
    :random.seed(:os.timestamp)
    Systems.Combat.start(entity)
    Systems.Combat.start(target, (:random.uniform(10) / 10) + 0.5)
  end

  def swing(entity) do
    Components.Combat.stop_timer(entity)
    swing(entity, targets(entity))
  end

  def swing(entity, []) do
    break_at = Components.Combat.value(entity)["break_at"]
               |> Date.convert :secs

    if break_at > Date.convert(Date.now, :secs) do
      start(entity)
    else
      send_message(entity, "scroll", "<p><span class='dark-yellow'>*Combat Off*</span></p>")
    end
  end

  def swing(entity, targets) when is_list targets do
    :random.seed(:os.timestamp)
    target = targets
             |> Enum.shuffle
             |> List.first

    delay = if stunned?(entity) do
      send_message(entity, "scroll", "<p><span class='yellow'>You are stunned and cannot attack!</span></p>")
      0.5
    else
      Abilities.Attack.execute(entity, target)
    end

    Components.Combat.set_break_at(entity)
    start(entity, delay)
  end

  def targets(entity) do
    hunted = Components.Hunting.value(entity)
             |> Enum.into(HashSet.new)

    present = entity
              |> Parent.of
              |> Systems.Room.living_in_room
              |> Enum.into(HashSet.new)

    HashSet.intersection(hunted, present)
    |> Enum.into([])
  end

  def stunned?(target) do
    target
    |> Components.Effects.value
    |> Map.values
    |> Enum.member? :stunned
  end

  def dodge?(accuracy, target) when is_pid(target) do
    if stunned?(target) do
      false
    else
      dodge_skill = Skills.Dodge.modified(target)
      dodge?(accuracy, dodge_skill)
    end
  end

  def dodge?(accuracy, dodge_skill) do
    chance = 30
    if dodge_skill > 0 do
      difference = dodge_skill - accuracy
      chance = if difference > 0 do
        chance + difference * 0.2
      else
        chance + difference * 0.3
      end

      :random.seed(:os.timestamp)
      :random.uniform(100) < trunc(chance)
    else
      false
    end
  end

  def parry?(accuracy, target) when is_pid(target) do
    if stunned?(target) do
      false
    else
      parry_skill = Skills.Parry.modified(target)
      parry?(accuracy, parry_skill)
    end
  end

  def parry?(accuracy, parry_skill) do
    chance = 30
    if parry_skill > 0 do
      difference = parry_skill - accuracy
      chance = if difference > 0 do
        chance + difference * 0.2
      else
        chance + difference * 0.3
      end

      :random.seed(:os.timestamp)
      :random.uniform(100) < trunc(chance)
    else
      false
    end
  end


end
