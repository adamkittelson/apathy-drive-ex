defmodule Commands.Drop do
  use ApathyDrive.Command

  def keywords, do: ["drop"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
  end

  def execute(%Monster{} = monster, arguments) do
    current_room = Monster.find_room(monster)

    if Enum.any? arguments do
      item = Enum.join(arguments, " ")
      case Systems.Match.one(monster.inventory, :name_contains, item) do
        nil ->
          Monster.send_scroll(monster, "<p>You don't have \"#{item}\" to drop!</p>")
        %Item{} = match ->
          monster
          |> Monster.send_scroll("<p>You drop #{match.name}.</p>")
          |> Monster.drop_item(match)
      end
    else
      Monster.send_scroll(monster, "<p>Drop what?</p>")
    end
  end
end
