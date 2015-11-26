defmodule Commands.Look do
  require Logger
  use ApathyDrive.Command
  alias ApathyDrive.PubSub
  alias ApathyDrive.Mobile

  @directions ["n", "north", "ne", "northeast", "e", "east",
              "se", "southeast", "s", "south", "sw", "southwest",
               "w", "west", "nw", "northwest", "u", "up", "d", "down"]

  def keywords, do: ["look", "l"]

  def execute(mobile, arguments) do
    if Enum.any? arguments do
      cond do
        Enum.member?(@directions, Enum.join(arguments, " ")) ->
          ApathyDrive.Exit.look(mobile, Enum.join(arguments, " "))
        target = mobile |> find_mobile_in_room(Enum.join(arguments, " ")) ->
          look_at_mobile(mobile, target)
        target = mobile |> Mobile.room_id |> Room.find |> Room.find_item(Enum.join(arguments, " ")) ->
          look_at_item(mobile, target)
        target = mobile |> Mobile.find_item(Enum.join(arguments, " ")) ->
          look_at_item(mobile, target)
        true ->
          Mobile.send_scroll(mobile, "<p>You do not notice that here.</p>")
      end
    else
      look_at_room(mobile)
    end
  end

  def look_at_mobile(mobile, target) do
    %{name: name,
      description: description,
      hp_description: hp_description} = Mobile.get_look_data(target)


    Mobile.send_scroll(mobile, "<p><span class='cyan'>#{name}</span></p>")
    Mobile.send_scroll(mobile, "<p>#{description}</p>")
    Mobile.send_scroll(mobile, "<p>#{hp_description}</p>")
  end

  def look_at_room(mobile, room_id \\ nil) do
    if Mobile.blind?(mobile) do
      Mobile.send_scroll(mobile, "<p>You are blind.</p>")
    else
      room_id = room_id || Mobile.room_id(mobile)
      room_html = room_id
                  |> Room.find
                  |> Room.html(mobile)

      Mobile.send_scroll(mobile, room_html)
    end
  end

  defp find_mobile_in_room(mobile, string) do
    PubSub.subscribers("rooms:#{Mobile.room_id(mobile)}:mobiles")
    |> Systems.Match.one(:name_contains, string)
  end

  defp look_at_item(mobile, description) when is_binary(description) do
    Mobile.send_scroll mobile, "<p>#{description}</p>"
  end
  defp look_at_item(mobile, %{} = item) do
    Mobile.look_at_item(mobile, item)
  end

end
