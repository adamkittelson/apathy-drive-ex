defmodule ApathyDrive.Commands.Class do
  use ApathyDrive.Command

  def keywords do
    ApathyDrive.Class.names |> Enum.map(&String.downcase/1)
  end

  def execute(mobile, arguments) do
    message = sanitize(arguments)

    Mobile.class_chat(mobile, message)
  end

  def sanitize(arguments) do
    {:safe, message} = arguments
                       |> Enum.join(" ")
                       |> Phoenix.HTML.html_escape

    message
  end
end
