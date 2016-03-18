defmodule ApathyDrive.Commands.Inventory do
  use ApathyDrive.Command

  def keywords, do: ["i", "inv", "inventory"]

  def execute(mobile, _arguments) do
    Mobile.display_inventory(mobile)
  end

  def execute(%Mobile{spirit: nil}), do: nil
  def execute(%Mobile{spirit: %Spirit{inventory: inventory, equipment: equipment}} = mobile) do
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

    display_encumbrance(mobile)
  end

  defp display_encumbrance(%Mobile{spirit: nil}), do: nil

  defp display_encumbrance(%Mobile{} = mobile) do
    current = Mobile.current_encumbrance(mobile)
    max     = Mobile.max_encumbrance(mobile)
    percent = trunc((current / max) * 100)

    display_encumbrance(mobile, current, max, percent)
  end

  defp display_encumbrance(%Mobile{} = mobile, current, max, percent) when percent < 17 do
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>Encumbrance:</span> <span class='dark-cyan'>#{current}/#{max} -</span> None [#{percent}%]</p>")
  end

  defp display_encumbrance(%Mobile{} = mobile, current, max, percent) when percent < 34 do
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>Encumbrance:</span> <span class='dark-cyan'>#{current}/#{max} -</span> <span class='dark-green'>Light [#{percent}%]</span></p>")
  end

  defp display_encumbrance(%Mobile{} = mobile, current, max, percent) when percent < 67 do
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>Encumbrance:</span> <span class='dark-cyan'>#{current}/#{max} -</span> <span class='dark-yellow'>Medium [#{percent}%]</span></p>")
  end

  defp display_encumbrance(%Mobile{} = mobile, current, max, percent) do
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>Encumbrance:</span> <span class='dark-cyan'>#{current}/#{max} -</span> <span class='dark-red'>Heavy [#{percent}%]</span></p>")
  end
end
