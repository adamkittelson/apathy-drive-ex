defmodule ApathyDrive.Commands.Say do
  use ApathyDrive.Command
  alias ApathyDrive.{Match, Mobile, Monster}

  def keywords, do: ["say"]

  def execute(%Room{} = room, %Character{} = character, args) do
    message =
      args
      |> Enum.join(" ")
      |> Character.sanitize()

    room.mobiles
    |> Map.values()
    |> List.delete(character)
    |> Enum.each(fn
      %Character{} = observer when character != observer ->
        message =
          "<p>#{Mobile.colored_name(character)} says: <span class='dark-green'>\"#{message}\"</span></p>"

        Mobile.send_scroll(observer, "<p><span class='dark-magenta'>#{message}</span></p>")

      _ ->
        :noop
    end)

    Mobile.send_scroll(
      character,
      "<p>You say: <span class='dark-green'>\"#{message}\"</span></p>"
    )

    command_pets(room, character, message)
  end

  defp command_pets(room, character, message) do
    [target | command] = String.split(message, ", ")
    command = Enum.join(command, ", ")

    if String.ends_with?(target, "s") do
      target = String.replace_suffix(target, "s", "")

      room.mobiles
      |> Map.values()
      |> Enum.filter(&(Map.get(&1, :owner_id) == character.id))
      |> Match.all(:keyword_starts_with, target)
      |> Enum.reduce(room, fn pet, room ->
        command_pet(pet, command, character, room)
      end)
    else
      room.mobiles
      |> Map.values()
      |> Enum.filter(&(Map.get(&1, :owner_id) == character.id))
      |> Match.one(:keyword_starts_with, target)
      |> command_pet(command, character, room)
    end
  end

  defp command_pet(%Monster{} = pet, "follow", character, room) do
    Mobile.send_scroll(character, "<p>#{Mobile.colored_name(pet)} begins to follow you.</p>")
    put_in(room.mobiles[pet.ref].follow, true)
  end

  defp command_pet(%Monster{} = pet, "stay", character, room) do
    Mobile.send_scroll(character, "<p>#{Mobile.colored_name(pet)} will stay here.</p>")
    put_in(room.mobiles[pet.ref].follow, false)
  end

  defp command_pet(%Monster{} = pet, command, character, room) do
    Mobile.send_scroll(character, "<p>Commanding #{pet.name} to #{command}.</p>")
    room
  end

  defp command_pet(_, _command, _character, room), do: room
end
