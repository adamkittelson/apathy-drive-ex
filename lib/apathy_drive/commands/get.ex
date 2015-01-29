defmodule Commands.Get do
  use ApathyDrive.Command

  def keywords, do: ["get"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
  end

  def execute(_spirit, monster, arguments) do
    current_room = Parent.of(monster)

    if Enum.any? arguments do
      item = Enum.join(arguments, " ")
      case Systems.Match.one(Components.Items.get_items(current_room), :name_contains, item) do
        nil ->
          send_message(monster, "scroll", "<p>You don't see \"#{item}\" here.</p>")
        match ->
          get_item(monster, match, Components.Module.value(match).can_pick_up?)
      end
    else
      send_message(monster, "scroll", "<p>Get what?</p>")
    end
  end

  def get_item(monster, item, true) do
    current_room = Parent.of(monster)

    Components.Items.remove_item(current_room, item)
    Components.Items.add_item(monster, item)
    Entities.save!(monster)
    Entities.save(current_room)
    send_message(monster, "scroll", "<p>You get #{Components.Name.value(item)}.</p>")
  end

  def get_item(monster, item, false) do
    send_message(monster, "scroll", "<p>#{Components.Name.value(item) |> capitalize_first} cannot be picked up.</p>")
  end
end
