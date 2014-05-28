defmodule Commands.Help do
  use Systems.Command

  def keywords, do: ["help"]

  def execute(entity, arguments) do
    keyword = Enum.join(arguments, " ")
    help_info = Systems.Help.find(keyword)
    if help_info do
      Components.Player.send_message(entity, ["scroll", help_info])
    else
      Components.Player.send_message(entity, ["scroll", "<p>Sorry, no help is available for \"#{Enum.join(arguments, " ")}\".</p>"])
    end
  end
end
