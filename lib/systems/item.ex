defmodule Systems.Item do

  def spawn_item(item) do
    {:ok, entity} = ApathyDrive.Entity.init
    ApathyDrive.Entity.add_component(entity, Components.Name,        Components.Name.get_name(item))
    ApathyDrive.Entity.add_component(entity, Components.Description, Components.Description.get_description(item))
    ApathyDrive.Entity.save!(entity)
    entity
  end

  def spawn_item(item, entity) do
    item = spawn_item(item)
    Components.Items.add_item(entity, item)
    ApathyDrive.Entity.save!(entity)
  end

  def display_inventory(character) do
    items = Components.Items.get_items(character) |> Enum.map(&(Components.Name.value(&1))) |> Enum.join(", ")
    Components.Player.send_message(character, ["scroll", "<p>You are carrying #{items}</p>"])
  end
end
