defmodule Commands.Demon do
  use ApathyDrive.Command

  def keywords, do: ["demon"]

  # def execute(%Spirit{faction: "Demon"} = spirit, arguments) do
  #   message = sanitize(arguments)
  #   ApathyDrive.PubSub.broadcast!("chat:Demon", {:demon, spirit.name, message})
  #   spirit
  # end
  # 
  # def execute(%Spirit{} = spirit, _arguments) do
  #   Spirit.send_scroll(spirit, "<p>You are not a <span class='magenta'>Demon</span>!</p>")
  # end
  # 
  # def execute(%Monster{spirit: %Spirit{faction: "Demon"} = spirit} = monster, arguments) do
  #   message = sanitize(arguments)
  #   ApathyDrive.PubSub.broadcast!("chat:Demon", {:demon, spirit.name, message})
  #   monster
  # end
  # 
  # def execute(%Monster{} = monster, _arguments) do
  #   Monster.send_scroll(monster, "<p>You are not a <span class='magenta'>Demon</span>!</p>")
  # end
  # 
  # def sanitize(arguments) do
  #   {:safe, message} = arguments
  #                      |> Enum.join(" ")
  #                      |> Phoenix.HTML.html_escape
  # 
  #   message
  # end
end
