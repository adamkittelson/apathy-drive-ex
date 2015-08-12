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
        # target = current_room |> find_item_in_room(Enum.join(arguments, " ")) ->
        #   Spirit.send_scroll spirit, "<p>#{target}</p>"
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

  defp find_item_in_room(%Room{item_descriptions: item_descriptions}, string) do
    visible_item = item_descriptions["visible"]
                   |> Map.keys
                   |> Enum.map(&(%{name: &1, keywords: String.split(&1)}))
                   |> Systems.Match.one(:keyword_starts_with, string)

    hidden_item = item_descriptions["hidden"]
                  |> Map.keys
                  |> Enum.map(&(%{name: &1, keywords: String.split(&1)}))
                  |> Systems.Match.one(:keyword_starts_with, string)

    cond do
      visible_item ->
        item_descriptions["visible"][visible_item.name]
      hidden_item ->
        item_descriptions["hidden"][hidden_item.name]
      true ->
        nil
    end
  end

end
