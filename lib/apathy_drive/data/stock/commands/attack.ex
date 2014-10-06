defmodule Commands.Attack do
  use Systems.Command

  def keywords, do: ["attack", "a", "kill", "k"]

  def execute(spirit, nil, arguments) do
    send_message(spirit, "scroll", "<p>You need a body to do that.</p>")
  end

  def execute(spirit, monster, arguments) do
    current_room = Parent.of(monster)

    target = current_room |> find_entity_in_room(Enum.join(arguments, " "))
    attack(monster, target)
  end

  defp attack(entity, nil) do
    send_message(entity, "scroll", "<p>Attack what?</p>")
  end

  defp attack(entity, target) when entity == target do
    send_message(entity, "scroll", "<p>Attack yourself?</p>")
  end

  defp attack(entity, target) do
    send_message(entity, "scroll", "<p><span class='dark-yellow'>You move to attack #{Components.Name.value(target)}!</span></p>")
    Systems.Combat.attack(entity, target)
  end

  defp find_entity_in_room(room, string) do
    room
    |> Systems.Room.living_in_room
    |> Systems.Match.first(:name_contains, string)
  end

end
