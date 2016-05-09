defmodule ApathyDrive.Commands.Who do
  use ApathyDrive.Command
  alias ApathyDrive.{Mobile, Presence}

  def keywords, do: ["who"]

  def execute(mobile, _arguments) do
    Mobile.send_scroll(mobile, "<p><span class='dark-cyan'>Name</span></p>")
    Mobile.send_scroll(mobile, "<p><span class='dark-green'>==============================================================</span></p>")

    "spirits:online"
    |> Presence.metas
    |> Enum.each(fn
         %{name: name} ->
           Mobile.send_scroll(mobile, "<p>#{name}</p>")
       end)
  end
end
