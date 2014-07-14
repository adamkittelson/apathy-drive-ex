defmodule Commands.Attack do
  use Systems.Command

  def keywords, do: ["attack", "a", "kill", "k"]

  def execute(entity, arguments) do
    if Components.Spirit.value(entity) do
      send_message(entity, "scroll", "<p>You need a body to do that.</p>")
    else
      current_room = Parent.of(entity)

      target = current_room |> find_entity_in_room(Enum.join(arguments, " "))
      attack(entity, target)
    end
  end

  defp attack(entity, nil) do
    send_message(entity, "scroll", "<p>Attack what?</p>")
  end

  defp attack(entity, target) when entity == target do
    send_message(entity, "scroll", "<p>Attack yourself?</p>")
  end

  defp attack(entity, target) do
    send_message(entity, "scroll", "<p><span class='dark-yellow'>You move to attack #{Components.Name.value(target)}!</span></p>")
    Components.Hunting.add(entity, target)
    Systems.Combat.start(entity)
  end

  defp find_entity_in_room(room, string) do
    room
    |> Systems.Room.living_in_room
    |> Systems.Match.first(:name_contains, string)
  end

end
