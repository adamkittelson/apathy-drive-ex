defmodule ApathyDrive.Commands.Say do
  use ApathyDrive.Command

  def keywords, do: ["say"]

  def execute(mobile, arguments) do
    message = sanitize(arguments)

    ApathyDrive.PubSub.broadcast_from! mobile, "rooms:#{Mobile.room_id(mobile)}:mobiles", {:say, Mobile.say_data(mobile), message}

    Mobile.send_scroll(mobile, "<p>You say: <span class='dark-green'>\"#{message}\"</span></p>")
  end

  def sanitize(arguments) do
    {:safe, message} = arguments
                       |> Enum.join(" ")
                       |> Phoenix.HTML.html_escape

    message
  end

end
