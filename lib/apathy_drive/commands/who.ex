defmodule ApathyDrive.Commands.Who do
  use ApathyDrive.Command
  alias ApathyDrive.{Monster, Presence}

  def keywords, do: ["who"]

  def execute(%Room{} = room, %Monster{} = monster, _arguments) do
    Monster.send_scroll(monster, "<p><span class='dark-cyan'>Name</span></p>")
    Monster.send_scroll(monster, "<p><span class='dark-green'>==============================================================</span></p>")

    "spirits:online"
    |> Presence.metas
    |> Enum.each(fn
         %{name: name} ->
           Monster.send_scroll(monster, "<p>#{name}</p>")
       end)

    room
  end
end
