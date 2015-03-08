defmodule Commands.Gimme do
  use ApathyDrive.Command

  def keywords, do: ["gimme"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
  end

  def execute(%Monster{} = monster, arguments) do
    item_name = Enum.join(arguments, " ")

    case find_item(item_name, String.match?(item_name, ~r/^\d+$/)) do
      nil ->
        Monster.send_scroll(monster, "<p>Couldn't find an item called #{item_name}.</p>")
      item_template ->
        ItemTemplate.spawn_item(item_template, monster)
        Monster.send_scroll(monster, "<p>A #{ItemTemplate.value(item_template).name} appears in your inventory.</p>")
    end
  end

  def find_item(item_name, true) do
    item_name
    |> String.to_integer
    |> ItemTemplate.find
  end

  def find_item(item_name, false) do
    item_name
    |> ItemTemplate.find_by_name
    |> ItemTemplate.find
  end

end
