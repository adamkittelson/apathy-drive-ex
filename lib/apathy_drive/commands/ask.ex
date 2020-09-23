defmodule ApathyDrive.Commands.Ask do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Match}

  def keywords, do: ["ask"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(character, "<p>Ask whom?</p>")
    room
  end

  def execute(%Room{} = room, %Character{} = character, [_]) do
    Mobile.send_scroll(character, "<p>Ask what??</p>")
    room
  end

  def execute(%Room{} = room, %Character{} = character, arguments) do
    [query | question] = arguments

    question =
      question
      |> Enum.join(" ")
      |> String.downcase()

    target =
      room.mobiles
      |> Map.values()
      |> Match.one(:name_contains, String.downcase(query))

    ask(room, character, target, question)
  end

  def ask(%Room{} = room, %Character{} = character, nil, _question) do
    Mobile.send_scroll(character, "<p>Ask whom?</p>")
    room
  end

  def ask(%Room{} = room, %Character{} = character, %{} = target, _question)
      when character == target do
    Mobile.send_scroll(character, "<p>Ask yourself?</p>")
    room
  end

  def ask(%Room{} = room, %Character{} = character, %{} = target, question) do
    name =
      target.base_name
      |> String.split(~r/[^\w]+/)
      |> Enum.map(&Macro.camelize/1)
      |> Enum.join()

    module = Module.concat([ApathyDrive, Scripts, name])

    if function_exported?(module, :ask, 4) do
      module.ask(room, character.ref, target.ref, question)
    else
      Mobile.send_scroll(
        character,
        "<p><span class='dark-green'>#{target.name} has nothing to tell you!</span></p>"
      )

      room
    end
  end
end
