defmodule Commands.Remove do
  use ApathyDrive.Command

  def keywords, do: ["remove", "unequip", "unwield"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
  end

  def execute(_spirit, monster, arguments) do
    Systems.Item.unequip(monster, Enum.join(arguments, " "))
  end
end
