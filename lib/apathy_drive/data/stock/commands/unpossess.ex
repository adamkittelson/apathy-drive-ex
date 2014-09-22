defmodule Commands.Unpossess do
  use Systems.Command

  def keywords, do: ["unpossess"]

  def execute(entity, arguments) do
    possessed = Possession.possessed(entity)

    if possessed do
      Possession.unpossess(entity)
      send_message(entity, "scroll", "<p>You leave the body of #{Components.Name.value(possessed)}.</p>")
    else
      send_message(entity, "scroll", "<p>You aren't possessing anything.</p>")
    end
  end

end
