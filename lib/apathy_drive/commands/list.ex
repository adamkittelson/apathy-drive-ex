defmodule ApathyDrive.Commands.List do
  use ApathyDrive.Command

  def keywords, do: ["list"]

  def execute(mobile, _arguments) do
    Mobile.send_scroll(mobile, "<p><span class='dark-yellow'>You care not for mortal trinkets.</span></p>")
  end
end
