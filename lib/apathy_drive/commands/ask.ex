defmodule Commands.Ask do
  use ApathyDrive.Command

  def keywords, do: ["ask"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
  end

  def execute(_spirit, monster, []),          do: send_message(monster, "scroll", "<p>Ask whom?</p>")
  def execute(_spirit, monster, [_question]), do: send_message(monster, "scroll", "<p>Ask what?</p>")

  def execute(_spirit, monster, arguments) do
    current_room = Parent.of(monster)

    [target | question] = arguments

    target = current_room |> find_entity_in_room(target)

    ask(monster, target, Enum.join(question, " "))
  end

  def ask(monster, nil, _question), do: send_message(monster, "scroll", "<p>Ask whom?</p>")
  def ask(monster, target, question) do
    questions = Components.Module.value(target).questions

    if questions |> Map.keys |> Enum.member?(question) do
      Systems.Script.execute(questions[question], monster)
    else
      send_message(monster, "scroll", "<p><span class='dark-green'>#{Components.Name.value(target)} has nothing to tell you!</span></p>")
    end
  end

  defp find_entity_in_room(room, string) do
    room
    |> Systems.Room.living_in_room
    |> Systems.Match.one(:name_contains, string)
  end

end
