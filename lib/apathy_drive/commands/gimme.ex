defmodule Commands.Gimme do
  use ApathyDrive.Command

  def keywords, do: ["gimme"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
  end

  def execute(spirit, monster, arguments) do
    item_name = Enum.join(arguments, " ")

    it = ItemTemplates.find_by_id(item_name)

    if it do
      Systems.Item.spawn_item(it, monster)
      Spirit.send_scroll(spirit, "<p>A #{Components.Name.value(it)} appears in your inventory.</p>")
      Entities.save(monster)
    else
      Spirit.send_scroll(spirit, "<p>Couldn't find an item called #{item_name}'.</p>")
    end
  end

end
