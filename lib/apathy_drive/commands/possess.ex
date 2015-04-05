defmodule Commands.Possess do
  use ApathyDrive.Command

  def keywords, do: ["possess"]

  def execute(%Monster{} = monster, _arguments) do
    Monster.send_scroll(monster, "<p>You must unpossess #{monster.name} first.</p>")
  end

  def execute(%Spirit{} = spirit, []) do
    spirit
    |> Spirit.send_scroll("<p>Possess what?</p>")
  end

  def execute(%Spirit{room_id: room_id} = spirit, arguments) do
    monsters = ApathyDrive.PubSub.subscribers("rooms:#{room_id}:monsters")

    case Systems.Match.one(monsters, :name_contains, Enum.join(arguments, " ")) do
      nil ->
        Spirit.send_scroll(spirit, "<p>You do not notice that here.</p>")
      monster ->
        send(monster, {:possession, spirit})
        spirit
    end
  end

end
