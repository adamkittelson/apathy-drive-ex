defmodule Commands.Unpossess do
  use Systems.Command

  def keywords, do: ["unpossess"]

  def execute(entity, arguments) do
    possessor = Possession.possessor(entity)

    if possessor do
      Possession.unpossess(entity)
      send_message(entity, "scroll", "<p>You leave the body of #{Components.Name.value(entity)}.</p>")
    else
      send_message(entity, "scroll", "<p>You aren't possessing anything.</p>")
    end
  end

end
