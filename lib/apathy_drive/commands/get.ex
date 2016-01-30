defmodule Commands.Get do
  use ApathyDrive.Command

  def keywords, do: ["get"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Get what?</p>")
  end
  def execute(mobile, ["all"]) do
    room =
      mobile
      |> Mobile.room_id
      |> Room.find

    room
    |> Room.item_names
    |> Enum.each(fn(item_name) ->
         execute(mobile, [item_name])
       end)
  end
  def execute(mobile, arguments) do
    item = Enum.join(arguments, " ")

    room =
      mobile
      |> Mobile.room_id
      |> Room.find

    case Room.get_item(room, item) do
      {:cant_get, item_name} ->
        Mobile.send_scroll(mobile, "<p>#{item_name |> capitalize_first} cannot be picked up.</p>")
      :not_found ->
        Mobile.send_scroll(mobile, "<p>You don't see \"#{item}\" here.</p>")
      %{} = item ->
        case Mobile.get_item(mobile, item) do
          :ok ->
            Mobile.send_scroll(mobile, "<p>You get #{item["name"]}.</p>")
          :too_heavy ->
            Room.add_item(room, item)
            Mobile.send_scroll(mobile, "<p>#{capitalize_first(item["name"])} is too heavy.</p>")
        end
    end
  end
end
