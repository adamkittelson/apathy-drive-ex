defmodule Commands.Construct do
  use ApathyDrive.Command

  def keywords, do: ["construct"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Construct what?</p>")
  end
  def execute(mobile, arguments) do
    item = Enum.join(arguments, " ")

    case Mobile.construct_item(mobile, item) do
      {:ok, item_name, cost} ->
        Mobile.send_scroll(mobile, "<p>You construct a #{item_name} using #{cost} of your essence.</p>")
      {:too_heavy, item_name} ->
        Mobile.send_scroll(mobile, "<p>A #{item_name} would be too heavy for you to hold.</p>")
      {:not_enough_essence, item_name} ->
        Mobile.send_scroll(mobile, "<p>You don't have enough essence to construct #{item_name}.</p>")
      :possessed ->
        Mobile.send_scroll(mobile, "<p>You can't construct items while using possession.</p>")
      :not_found ->
        Mobile.send_scroll(mobile, "<p>You don't know how to construct a #{item}.</p>")
    end
  end
end
