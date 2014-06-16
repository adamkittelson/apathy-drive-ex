defmodule Help.Races do
  use Systems.Help

  def keywords, do: ["races"]

  def help do
    Races.all
    |> Enum.sort(fn(race1, race2) ->
         Components.Module.value(race1).cost <= Components.Module.value(race2).cost
       end)
    |> Enum.map(fn(race) ->
         "#{String.ljust(Components.Name.value(race), 20)} Cost: #{Components.Module.value(race).cost}"
       end)
    |> Enum.join("\n")
  end
end
