defmodule ApathyDrive.Commands.Inventory do
  use ApathyDrive.Command

  def keywords, do: ["i", "inv", "inventory"]

  def execute(%Room{} = room, %Mobile{spirit: %Spirit{inventory: inventory, equipment: equipment}} = mobile, _args) do
    if equipment |> Enum.any? do
      Mobile.send_scroll(mobile, "<p><span class='dark-yellow'>You are equipped with:</span></p><br>")

      equipment
      |> Enum.each(fn(item) ->
           Mobile.send_scroll(mobile, "<p><span class='dark-green'>#{String.ljust(item["name"], 23)}</span><span class='dark-cyan'>(#{item["worn_on"]})</span></p>")
         end)
      Mobile.send_scroll(mobile, "<br>")
    end

    items = inventory |> Enum.map(&(&1["name"]))
    if items |> Enum.count > 0 do
      Mobile.send_scroll(mobile, "<p>You are carrying #{Enum.join(items, ", ")}</p>")
    else
      Mobile.send_scroll(mobile, "<p>You are carrying nothing.</p>")
    end

    room
  end

end
