defmodule Commands.Elemental do
  use ApathyDrive.Command

  def keywords, do: ["elemental"]

  # def execute(%Spirit{faction: "Elemental"} = spirit, arguments) do
  #   message = sanitize(arguments)
  #   ApathyDrive.PubSub.broadcast!("chat:Elemental", {:elemental, spirit.name, message})
  #   spirit
  # end
  # 
  # def execute(%Spirit{} = spirit, _arguments) do
  #   Spirit.send_scroll(spirit, "<p>You are not an <span class='dark-cyan'>Elemental</span>!</p>")
  # end
  # 
  # def execute(%Monster{spirit: %Spirit{faction: "Elemental"} = spirit} = monster, arguments) do
  #   message = sanitize(arguments)
  #   ApathyDrive.PubSub.broadcast!("chat:Elemental", {:elemental, spirit.name, message})
  #   monster
  # end
  # 
  # def execute(%Monster{} = monster, _arguments) do
  #   Monster.send_scroll(monster, "<p>You are not an <span class='dark-cyan'>Elemental</span>!</p>")
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
