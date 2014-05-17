defmodule Systems.Item do

  def spawn_item(item) do
    {:ok, entity} = ApathyDrive.Entity.init
    ApathyDrive.Entity.list_components(item)
    |> Enum.reject(&(&1 == Components.ID))
    |> Enum.each fn(component) ->
      ApathyDrive.Entity.add_component(entity, component, component.value(item))
    end
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
