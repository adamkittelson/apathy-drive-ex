defmodule ApathyDrive.Commands.Top do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Mobile, Repo}

  def keywords, do: ["top"]

  def execute(%Room{} = room, %Character{} = character, []) do
    top(character, 10)
    room
  end

  def execute(%Room{} = room, %Character{} = character, [number | _rest]) do
    case Integer.parse(number) do
      {number, ""} ->
        top(character, number)

      _ ->
        top(character)
    end

    room
  end

  def top(character, number \\ 10) do
    Mobile.send_scroll(
      character,
      "<p><span class='dark-red'>Rank</span> <span class='dark-green'>Name</span>                  <span class='dark-magenta'>Class</span>         <span class='dark-yellow'>Experience</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-grey'>=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=</span></p>"
    )

    number
    |> Character.top_list()
    |> Enum.map(fn %{character: character, class: class} ->
      exp =
        character
        |> Ecto.assoc(:character_classes)
        |> Repo.all()
        |> Enum.map(& &1.experience)
        |> Enum.sum()

      %{name: character.name, class: class, total_exp: trunc(exp)}
    end)
    |> Enum.sort_by(&(-&1.total_exp))
    |> Enum.with_index()
    |> Enum.each(fn {%{name: name, total_exp: exp, class: class}, index} ->
      index = (index + 1) |> to_string |> String.pad_leading(3)
      name = String.pad_trailing(name, 22)
      class = String.pad_trailing(class, 14)

      Mobile.send_scroll(
        character,
        "<p><span class='dark-red'>#{index}.</span> <span class='dark-green'>#{name}</span><span class='dark-magenta'>#{
          class
        }</span><span class='dark-yellow'>#{exp}</span></p>"
      )
    end)
  end
end
