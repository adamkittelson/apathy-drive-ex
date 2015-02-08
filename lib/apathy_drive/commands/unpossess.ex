defmodule Commands.Unpossess do
  use ApathyDrive.Command

  def keywords, do: ["unpossess"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You aren't possessing anything.</p>")
  end

  def execute(%Monster{} = monster, _arguments) do
    Phoenix.PubSub.broadcast("monsters:#{monster.id}", :unpossess)
    monster
  end

end
