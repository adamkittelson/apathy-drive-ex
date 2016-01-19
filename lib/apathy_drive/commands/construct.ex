defmodule Commands.Construct do
  use ApathyDrive.Command

  def keywords, do: ["construct"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Construct what?</p>")
  end
  def execute(mobile, arguments) do
    item = Enum.join(arguments, " ")

    Mobile.construct_item(mobile, item)
  end
end
