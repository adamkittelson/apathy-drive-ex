defmodule Commands.Look do
  require Logger
  use ApathyDrive.Command
  import Systems.Text
  alias ApathyDrive.{Mobile, PubSub, World, Match}

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
    target = World.mobile(target)

    hp_percentage = round(100 * (target.hp / target.max_hp))

    hp_description = case hp_percentage do
      _ when hp_percentage >= 100 ->
        "unwounded"
      _ when hp_percentage >= 90 ->
        "slightly wounded"
      _ when hp_percentage >= 60 ->
        "moderately wounded"
      _ when hp_percentage >= 40 ->
        "heavily wounded"
      _ when hp_percentage >= 20 ->
        "severely wounded"
      _ when hp_percentage >= 10 ->
        "critically wounded"
      _ ->
        "very critically wounded"
    end

    hp_description =
      "{{target:He/She/It}} appears to be #{hp_description}."
      |> interpolate(%{"target" => target})

    Mobile.send_scroll(mobile, "<p><span class='cyan'>#{target.name}</span></p>")
    Mobile.send_scroll(mobile, "<p>#{target.description}</p>")
    Mobile.send_scroll(mobile, "<p>#{hp_description}</p>")
  end

  def look_at_room(mobile, room_id \\ nil) do
    if Mobile.blind?(mobile) do
      Mobile.send_scroll(mobile, "<p>You are blind.</p>")
    else
      room_id = room_id || mobile.room_id
      room_html = room_id
                  |> Room.find
                  |> Room.look_at_room(mobile)
    end
  end

  defp find_mobile_in_room(mobile, string) do
    PubSub.subscribers("rooms:#{Mobile.room_id(mobile)}:mobiles")
    |> Match.one(:name_contains, string)
  end

  defp look_at_item(mobile, description) when is_binary(description) do
    Mobile.send_scroll mobile, "<p>#{description}</p>"
  end
  defp look_at_item(mobile, %{} = item) do
    Mobile.look_at_item(mobile, item)
  end

end
