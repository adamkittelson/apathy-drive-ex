defmodule Commands.List do
  use ApathyDrive.Command

  def keywords, do: ["list"]

  def execute(%Spirit{} = spirit, _arguments) do
    Spirit.send_scroll(spirit, "<p><span class='dark-yellow'>You care not for mortal trinkets.</span></p>")
  end

  def execute(%Monster{} = monster, _arguments) do
    Monster.send_scroll(monster, "<p><span class='dark-yellow'>You care not for mortal trinkets.</span></p>")
  end
end
