defmodule Commands.Drop do
  use ApathyDrive.Command

  def keywords, do: ["drop"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Drop what?</p>")
  end
  def execute(mobile, arguments) do
    item = Enum.join(arguments, " ")

    case Mobile.drop_item(mobile, item) do
      {:ok, %{"name" => name} = item} ->
        mobile
        |> Mobile.room_id
        |> Room.find
        |> Room.add_item(item)

        Mobile.send_scroll(mobile, "<p>You drop #{name}.</p>")
      :not_found ->
        Mobile.send_scroll(mobile, "<p>You don't have \"#{item}\" to drop!</p>")
    end
  end
end
