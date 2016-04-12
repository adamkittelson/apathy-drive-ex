defmodule ApathyDrive.Commands.Remove do
  use ApathyDrive.Command

  def keywords, do: ["remove", "unequip", "unwield"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Remove what?</p>")
  end
  def execute(mobile, arguments) do
    item = Enum.join(arguments, " ")

    case Mobile.unequip_item(mobile, item) do
      {:ok, %{unequipped: unequipped}} ->
        Mobile.send_scroll(mobile, "<p>You remove #{unequipped["name"]}.</p>")
      :not_found ->
        Mobile.send_scroll(mobile, "<p>You don't have \"#{item}\" equipped.</p>")
    end
  end
end
