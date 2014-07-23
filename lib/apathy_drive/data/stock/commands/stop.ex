defmodule Commands.Stop do
  use Systems.Command

  def keywords, do: ["stop"]

  def execute(entity, arguments) do
    if Components.Spirit.value(entity) == true do
      send_message(entity, "scroll", "<p>You need a body to do that.</p>")
    else
      if Enum.join(arguments, " ") == "hunting" do
        send_message(entity, "scroll", "<p>You stop hunting.</p>")
        Components.Hunting.value(entity, [])
      end
    end
  end
end
