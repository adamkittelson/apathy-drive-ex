defmodule ApathyDrive.Commands.Who do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Mobile, Presence}

  def keywords, do: ["who"]

  def execute(%Room{} = room, %Character{} = character, _arguments) do
    Mobile.send_scroll(character, "<p><span class='yellow'>        Current Adventurers</span></p>")
    Mobile.send_scroll(character, "<p><span class='dark-grey'>        ===================</span>\n\n</p>")

    list = Presence.metas("spirits:online")

    longest_name_length =
      list
      |> Enum.max_by(&String.length(&1.name))
      |> Map.get(:name)
      |> String.length

    list
    |> Enum.sort_by(&(&1.name))
    |> Enum.each(fn
        %{name: name} ->
          alignment = "Good" |> String.pad_leading(7)
          name = name |> String.pad_trailing(longest_name_length)
          title = "Adventurer"
          Mobile.send_scroll(character, "<p>#{alignment} <span class='dark-green'>#{name} - <span class='dark-magenta'>#{title}</span></p>")
        end)

    room
  end
end
