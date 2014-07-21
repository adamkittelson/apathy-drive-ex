defmodule Systems.Combat do
  use Systems.Reload
  import Utility
  use Timex

  def start(entity) do
    Components.Combat.set_timer entity, delay(2, do: swing(entity))
  end

  def attack(entity, target) do
    Components.Hunting.add(entity, target)
    Components.Hunting.add(target, entity)
    Systems.Combat.start(entity)
    Systems.Combat.start(target)
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

    Abilities.Attack.execute(entity, target)
    Components.Combat.set_break_at(entity)
    start(entity)
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

end
