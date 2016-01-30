defmodule Commands.Wear do
  use ApathyDrive.Command

  def keywords, do: ["wear", "equip", "wield"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Equip what?</p>")
  end
  def execute(mobile, ["all"]) do
    mobile
    |> Mobile.inventory_item_names
    |> Enum.each(fn(item_name) ->
         execute(mobile, [item_name])
       end)
  end
  def execute(mobile, arguments) do
    item = Enum.join(arguments, " ")

    case Mobile.equip_item(mobile, item) do
      {:ok, %{equipped: equipped, unequipped: unequipped}} ->
        Enum.each(unequipped, fn(item) ->
          Mobile.send_scroll(mobile, "<p>You remove #{item["name"]}.</p>")
        end)
        Mobile.send_scroll(mobile, "<p>You are now wearing #{equipped["name"]}.</p>")
      {:ok, %{equipped: equipped}} ->
        Mobile.send_scroll(mobile, "<p>You are now wearing #{equipped["name"]}.</p>")
      :not_found ->
        Mobile.send_scroll(mobile, "<p>You don't have \"#{item}\" left unequipped.</p>")
    end
  end
end