defmodule Commands.Unpossess do
  use Systems.Command

  def keywords, do: ["unpossess"]

  def execute(spirit, nil, arguments) do
    send_message(spirit, "scroll", "<p>You aren't possessing anything.</p>")
  end

  def execute(spirit, monster, arguments) do
    Possession.unpossess(spirit)
    send_message(spirit, "scroll", "<p>You leave the body of #{Components.Name.value(monster)}.</p>")
    Systems.Prompt.update(spirit, nil)
  end

end
