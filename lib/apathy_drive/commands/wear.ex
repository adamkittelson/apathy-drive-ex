defmodule ApathyDrive.Commands.Wear do
  use ApathyDrive.Command
  alias ApathyDrive.Match

  def keywords, do: ["wear", "equip", "wield"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Equip what?</p>")
  end

  def execute(mobile, arguments) when is_pid(mobile) do
    item = Enum.join(arguments, " ")

    Mobile.equip_item(mobile, item)
  end

  def execute(%Mobile{spirit: %Spirit{inventory: inventory}} = mobile, "all") do
    inventory
    |> Enum.map(&(&1["name"]))
    |> Enum.each(fn(item_name) ->
         Mobile.equip_item(self, item_name)
       end)
    mobile
  end

  def execute(%Mobile{spirit: %Spirit{inventory: inventory}} = mobile, item_name) do
    item = inventory
           |> Enum.map(&(%{name: &1["name"], keywords: String.split(&1["name"]), item: &1}))
           |> Match.one(:name_contains, item_name)

    case item do
      nil ->
       Mobile.send_scroll(mobile, "<p>You don't have \"#{item_name}\" left unequipped.</p>")
     %{item: item} ->
       case Mobile.equip_item(mobile, item) do
         %{equipped: equipped, unequipped: unequipped, mobile: mobile} ->
           Enum.each(unequipped, fn(item) ->
             Mobile.send_scroll(mobile, "<p>You remove #{item["name"]}.</p>")
           end)
           Mobile.send_scroll(mobile, "<p>You are now wearing #{equipped["name"]}.</p>")
           Mobile.save(mobile)
         %{equipped: equipped, mobile: mobile} ->
           Mobile.send_scroll(mobile, "<p>You are now wearing #{equipped["name"]}.</p>")
       end
    end
  end
end
