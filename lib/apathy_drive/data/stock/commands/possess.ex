defmodule Commands.Possess do
  use Systems.Command

  def keywords, do: ["possess"]

  def execute(entity, arguments) do
    current_room = Parent.of(entity)

    if Enum.any? arguments do
      if target = current_room |> find_entity_in_room(Enum.join(arguments, " ")) do
        Possession.possess(entity, target)
        send_message(entity, "scroll", "<p>You possess #{Components.Name.value(target)}.")
      else
        send_message(entity, "scroll", "<p>You do not notice that here.</p>")
      end
    else
      send_message(entity, "scroll", "<p>Possess what?.</p>")
    end
  end

  defp find_entity_in_room(room, string) do
    room
    |> Systems.Room.living_in_room
    |> Systems.Match.first(:name_contains, string)
  end

end
