defmodule Commands.Wear do
  use ApathyDrive.Command

  def keywords, do: ["wear", "equip", "wield"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
  end

  def execute(_spirit, monster, arguments) do
    Systems.Item.equip(monster, Enum.join(arguments, " "))
  end
end
