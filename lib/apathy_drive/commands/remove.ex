defmodule Commands.Remove do
  use ApathyDrive.Command

  def keywords, do: ["remove", "unequip", "unwield"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
  end

  def execute(%Monster{} = monster, arguments) do
    item = Enum.join(arguments, " ")
    case Systems.Match.one(Monster.equipped_items(monster), :name_contains, item) do
      nil ->
        Monster.send_scroll(monster, "<p>You don't have \"#{item}\" equipped.</p>")
      match ->
        Monster.unequip_item(monster, match)
    end
  end
end
