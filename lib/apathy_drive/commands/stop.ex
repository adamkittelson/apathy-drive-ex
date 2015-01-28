defmodule Commands.Stop do
  use ApathyDrive.Command

  def keywords, do: ["stop"]

  def execute(spirit, nil, arguments) do
    send_message(spirit, "scroll", "<p>You need a body to do that.</p>")
  end

  def execute(spirit, monster, arguments) do
    if Enum.join(arguments, " ") == "hunting" do
      send_message(spirit, "scroll", "<p>You stop hunting.</p>")
      Components.Hunting.value(monster, [])
    end
  end
end
