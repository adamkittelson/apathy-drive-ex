defmodule Commands.Possess do
  use Systems.Command

  def keywords, do: ["possess"]

  def execute(spirit, _monster, []) do
    send_message(spirit, "scroll", "<p>Possess what?.</p>")
  end

  def execute(spirit, monster, arguments) do
    current_room = Parent.of(spirit)

    if target = current_room |> find_entity_in_room(Enum.join(arguments, " ")) do
      possess(spirit, target)
    else
      send_message(spirit, "scroll", "<p>You do not notice that here.</p>")
    end
  end

  def possess(spirit, target) do
    if Systems.Trainer.total_power(spirit) >= Components.Module.value(target).required_power do
      Possession.possess(spirit, target)
      send_message(spirit, "scroll", "<p>You possess #{Components.Name.value(target)}.")
      Systems.Prompt.update(spirit, Possession.possessed(spirit))
    else
      send_message(spirit, "scroll", "<p>You aren't yet powerful enough to possess #{Components.Name.value(target)}.")
    end
  end

  defp find_entity_in_room(room, string) do
    room
    |> Systems.Room.living_in_room
    |> Systems.Match.one(:name_contains, string)
  end

end
