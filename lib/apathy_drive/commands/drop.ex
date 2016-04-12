defmodule ApathyDrive.Commands.Drop do
  use ApathyDrive.Command

  def keywords, do: ["drop"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Drop what?</p>")
  end

  def execute(mobile, arguments) do
    item = Enum.join(arguments, " ")

    Mobile.drop_item(mobile, item)
  end
end
