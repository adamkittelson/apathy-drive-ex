defmodule Commands.Wear do
  use ApathyDrive.Command

  def keywords, do: ["wear", "equip", "wield"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
  end

  def execute(%Monster{} = monster, arguments) do
    item = Enum.join(arguments, " ")
    case Systems.Match.one(Monster.inventory(monster), :name_contains, item) do
      nil ->
        Monster.send_scroll(monster, "<p>You don't have \"#{item}\" left unequipped.</p>")
      match ->
        Monster.equip_item(monster, match, ItemTemplate.skill_too_low?(monster, match))
    end
  end
end
