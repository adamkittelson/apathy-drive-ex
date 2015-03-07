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
      case Systems.Match.one(Monster.inventory(monster), :name_contains, item) do
        nil ->
          Monster.send_scroll(monster, "<p>You don't have \"#{item}\" to drop!</p>")
        %Item{} = match ->
          Item.to_room(match.pid, current_room)
          Monster.send_scroll(monster, "<p>You drop #{match.name}.</p>")
      end
    else
      Monster.send_scroll(monster, "<p>Drop what?</p>")
    end
  end
end
