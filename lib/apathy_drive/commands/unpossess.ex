defmodule Commands.Unpossess do
  use ApathyDrive.Command

  def keywords, do: ["unpossess"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You aren't possessing anything.</p>")
  end

  def execute(%Monster{spirit: spirit} = monster, _arguments) do

    PubSub.unsubscribe(self, "spirits:online")
    PubSub.unsubscribe(self, "spirits:hints")
    PubSub.unsubscribe(self, "chat:gossip")
    PubSub.unsubscribe(self, "chat:#{spirit.alignment}")

    spirit
    |> Map.put(:room_id, monster.room_id)
    |> Spirit.save

    spirit = Systems.Login.login(spirit.socket, spirit.id)

    spirit
    |> Spirit.send_scroll("<p>You leave the body of #{monster.name}.</p>")
    |> Systems.Prompt.update

    spirit
  end

end
