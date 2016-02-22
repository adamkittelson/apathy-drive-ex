defmodule Commands.Who do
  use ApathyDrive.Command
  alias ApathyDrive.{Mobile, World}

  def keywords, do: ["who"]

  def execute(mobile, _arguments) do
    Mobile.send_scroll(mobile, "<p><span class='dark-cyan'>Name                Location</span></p>")
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>==============================================================</span></p>")

    ApathyDrive.PubSub.subscribers("angel-unity")
    |> Enum.map(fn(pid) ->
         mob = World.mobile(pid)
         %{name: mob.name, room: World.room(mob.room_id).name, alignment: mob.alignment}
       end)
    |> Enum.each(fn(line) ->
         Mobile.send_scroll(mobile, format_line(line))
       end)
  end

  def format_line(%{name: name, room: room} = line) do
    color = Mobile.alignment_color(line)

    "<p><span class='#{color}'>#{String.ljust(name, 20)}</span><span class='cyan'>#{room}</span></p>"
  end

end
