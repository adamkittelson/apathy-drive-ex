defmodule Commands.Greet do
  use ApathyDrive.Command

  def keywords, do: ["greet"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
  end

  def execute(_spirit, monster, arguments) do
    current_room = Parent.of(monster)

    if Enum.any? arguments do
      target = current_room |> find_entity_in_room(Enum.join(arguments, " "))
      if target do
        if target == monster do
          send_message(monster, "scroll", "<p>Greet yourself?</p>")
        else
          greet(monster, target, current_room)
        end
      else
        send_message(monster, "scroll", "<p>Greet whom?</p>")
      end
    else
      send_message(monster, "scroll", "<p>Greet whom?</p>")
    end
  end

  defp find_entity_in_room(room, string) do
    room
    |> Systems.Room.living_in_room
    |> Systems.Match.one(:name_contains, string)
  end

  defp greet(monster, target, room) do
    room
    |> Systems.Room.characters_in_room
    |> Enum.each(fn(character) ->

      observer = Possession.possessed(character) || character

      cond do
        observer == monster ->
          send_message(character, "scroll", "<p><span class='dark-green'>#{Components.Module.value(target).greeting}</span></p>")
        observer == target ->
          send_message(character, "scroll", "<p><span class='dark-green'>#{Components.Name.value(monster) |> capitalize_first} greets you.</span></p>")
        true ->
          send_message(character, "scroll", "<p><span class='dark-green'>#{Components.Name.value(monster) |> capitalize_first} greets #{Components.Name.value(target)}.</span></p>")
      end
    end)
  end

end
