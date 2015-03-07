defmodule Commands.Extinguish do
  use ApathyDrive.Command

  def keywords, do: ["extinguish", "douse"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
  end

  def execute(%Monster{} = monster, arguments) do
    if Enum.any? arguments do
      cond do
        target = find_item_on_monster(monster, Enum.join(arguments, " ")) ->
          case Item.extinguish(target.pid) do
            :not_a_light ->
              Monster.send_scroll(monster, "<p>You can't extinguish a #{target.name}!</p>")
            :not_lit ->
              Monster.send_scroll(monster, "<p>The #{target.name} isn't lit!</p>")
            :always_lit ->
              Monster.send_scroll(monster, "<p>The #{target.name}'s light can not be extinguished!</p>")
            _ ->
              Monster.send_scroll(monster, "<p>You extinguish the #{target.name}.</p>")
          end
      true ->
        Monster.send_scroll(monster, "<p>You aren't carrying that.</p>")
      end
    else
      Monster.send_scroll(monster, "<p>Extinguish what?</p>")
    end
  end

  def find_item_on_monster(%Monster{} = monster, string) do
    (Monster.equipped_items(monster) ++ Monster.inventory(monster))
    |> Systems.Match.one(:name_contains, string)
  end

end
