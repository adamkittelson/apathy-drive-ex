defmodule Commands.Who do
  use ApathyDrive.Command
  alias ApathyDrive.{Mobile, World}

  def keywords, do: ["who"]

  def execute(mobile, _arguments) do
    Mobile.send_scroll(mobile, "<p><span class='dark-cyan'>Name</span></p>")
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>==============================================================</span></p>")

    ApathyDrive.PubSub.subscribers("spirits:online")
    |> Enum.map(fn(pid) ->
         mob = World.mobile(pid)
         %{name: mob.name, alignment: mob.alignment}
       end)
    |> Enum.each(fn(line) ->
         Mobile.send_scroll(mobile, format_line(line))
       end)
  end

  def format_line(%{name: name} = line) do
    color = Mobile.alignment_color(line)

    "<p><span class='#{color}'>#{String.ljust(name, 20)}</span></p>"
  end

end
