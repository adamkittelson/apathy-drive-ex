defmodule ApathyDrive.Commands.Inventory do
  use ApathyDrive.Command

  def keywords, do: ["i", "inv", "inventory"]

  def execute(%Room{} = room, %Monster{spirit: %Spirit{inventory: inventory, equipment: equipment}} = monster, _args) do
    if equipment |> Enum.any? do
      Monster.send_scroll(monster, "<p><span class='dark-yellow'>You are equipped with:</span></p><br>")

      equipment
      |> Enum.each(fn(item) ->
           Monster.send_scroll(monster, "<p><span class='dark-green'>#{String.ljust(item["name"], 23)}</span><span class='dark-cyan'>(#{item["worn_on"]})</span></p>")
         end)
      Monster.send_scroll(monster, "<br>")
    end

    items = inventory |> Enum.map(&(&1["name"]))
    if items |> Enum.count > 0 do
      Monster.send_scroll(monster, "<p>You are carrying #{Enum.join(items, ", ")}</p>")
    else
      Monster.send_scroll(monster, "<p>You are carrying nothing.</p>")
    end

    room
  end

end
