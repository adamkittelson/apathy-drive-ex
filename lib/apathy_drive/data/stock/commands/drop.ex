defmodule Commands.Drop do
  use Systems.Command

  def keywords, do: ["drop"]

  def execute(spirit, nil, _arguments) do
    send_message(spirit, "scroll", "<p>You need a body to do that.</p>")
  end

  def execute(_spirit, monster, arguments) do
    current_room = Parent.of(monster)

    if Enum.any? arguments do
      item = Enum.join(arguments, " ")
      case Systems.Match.one(Components.Items.get_items(monster), :name_contains, item) do
        nil ->
          send_message(monster, "scroll", "<p>You don't have \"#{item}\" to drop!</p>")
        match ->
          Components.Items.remove_item(monster, match)
          Components.Items.add_item(current_room, match)
          Entities.save!(monster)
          Entities.save(current_room)
          send_message(monster, "scroll", "<p>You drop #{Components.Name.value(match)}.</p>")
      end
    else
      send_message(monster, "scroll", "<p>Drop what?</p>")
    end
  end
end
