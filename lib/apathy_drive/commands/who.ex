defmodule ApathyDrive.Commands.Who do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Directory, Mobile}

  def keywords, do: ["who"]

  def execute(%Room{} = room, %Character{} = character, arguments) do
    who_game = Enum.join(arguments) |> String.downcase()

    Mobile.send_scroll(
      character,
      "<p><span class='yellow'>        Current Adventurers</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-grey'>        ===================</span>\n\n</p>"
    )

    list =
      Directory.list_characters()
      |> Enum.sort()

    longest_name_length =
      list
      |> Enum.max_by(&String.length(&1.name))
      |> Map.get(:name)
      |> String.length()

    list
    |> Enum.sort_by(&String.downcase(&1.name))
    |> Enum.each(fn
      %{name: name, game: game} ->
        if String.length(who_game) > 0 and String.starts_with?(String.downcase(game), who_game) do
          name = name |> String.pad_trailing(longest_name_length)

          Mobile.send_scroll(
            character,
            "<p>        <span class='dark-green'>#{name} -</span> <span class='dark-magenta'>Outlander</span> <span class='dark-green'>of</span> <span class='dark-yellow'>#{
              game
            }</span>"
          )
        end

      %{name: name, title: title, evil_points: evil_points} ->
        if who_game == "" do
          legal_status = Character.legal_status(%Character{evil_points: evil_points})
          color = color(legal_status)
          legal_status = legal_status |> String.pad_leading(7)
          legal_status = "<span class='#{color}'>#{legal_status}</span>"
          name = name |> String.pad_trailing(longest_name_length)

          Mobile.send_scroll(
            character,
            "<p>#{legal_status} <span class='dark-green'>#{name} - <span class='dark-magenta'>#{
              title
            }</span></p>"
          )
        end
    end)

    room
  end

  def color("Saint"), do: "white"
  def color("Good"), do: "white"
  def color("Neutral"), do: "dark-cyan"
  def color("Seedy"), do: "dark-grey"
  def color("Outlaw"), do: "dark-red"
  def color("Criminal"), do: "dark-yellow"
  def color("Villain"), do: "yellow"
  def color("FIEND"), do: "red"
end
