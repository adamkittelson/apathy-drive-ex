defmodule Commands.Get do
  use ApathyDrive.Command

  def keywords, do: ["get"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
  end

  def execute(%Monster{} = monster, arguments) do
    current_room = Monster.find_room(monster)

    if Enum.any? arguments do
      item = Enum.join(arguments, " ")
      case Systems.Match.one(Room.items(current_room), :name_contains, item) do
        nil ->
          Monster.send_scroll(monster, "<p>You don't see \"#{item}\" here.</p>")
        %Item{can_pick_up: true} = match ->
          if Monster.remaining_encumbrance(monster) >= match.weight do
            Item.to_monster_inventory(match.pid, monster)
            Monster.send_scroll(monster, "<p>You get #{match.name}.</p>")
          else
            Monster.send_scroll(monster, "<p>#{capitalize_first(match.name)} is too heavy.</p>")
          end
        %Item{} = match ->
          Monster.send_scroll(monster, "<p>#{match.name |> capitalize_first} cannot be picked up.</p>")
      end
    else
      Monster.send_scroll(monster, "<p>Get what?</p>")
    end
  end
end
