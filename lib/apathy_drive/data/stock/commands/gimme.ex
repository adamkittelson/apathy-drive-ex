defmodule Commands.Gimme do
  use Systems.Command

  def keywords, do: ["gimme"]


  def execute(spirit, nil, arguments) do
    send_message(spirit, "scroll", "<p>You need a body to do that.</p>")
  end

  def execute(spirit, monster, arguments) do
    current_room = Parent.of(spirit)
    item_name = Enum.join(arguments, " ")

    it = ItemTemplates.find_by_id(item_name)

    if it do
      Systems.Item.spawn_item(it, monster)
      send_message(spirit, "scroll", "<p>A #{Components.Name.value(it)} appears in your inventory.</p>")
      Entities.save(monster)
    else
      send_message(spirit, "scroll", "<p>Couldn't find an item called #{item_name}'.</p>")
    end
  end

end
