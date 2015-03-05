defmodule Commands.Light do
  use ApathyDrive.Command

  def keywords, do: ["light"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
  end

  def execute(%Monster{} = monster, arguments) do
    current_room = Monster.find_room(monster)

    if Enum.any? arguments do
      cond do
        target = find_item_on_monster(monster, Enum.join(arguments, " ")) ->
          case Item.light(target.pid) do
            :not_a_light ->
              Monster.send_scroll(monster, "<p>You can't light a #{target.name}!</p>")
            :already_lit ->
              Monster.send_scroll(monster, "<p>The #{target.name} is already lit!</p>")
            _ ->
              Monster.send_scroll(monster, "<p>You light the #{target.name}.</p>")
          end
      true ->
        Monster.send_scroll(monster, "<p>You aren't carrying that.</p>")
      end
    else
      Monster.send_scroll(monster, "<p>Light what?</p>")
    end
  end

  def find_item_on_monster(%Monster{} = monster, string) do
    (Monster.equipped_items(monster) ++ Monster.inventory(monster))
    |> Systems.Match.one(:name_contains, string)
  end

end
