defmodule Commands.Ask do
  use ApathyDrive.Command
  alias ApathyDrive.{PubSub, World}

  def keywords, do: ["ask"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Ask whom?</p>")
  end
  def execute(mobile, [_]) do
    Mobile.send_scroll(mobile, "<p>Ask what?</p>")
  end
  def execute(mobile, arguments) do
    [target | question] = arguments

    target = find_mobile_in_room(mobile, target)

    question =
      question
      |> Enum.join(" ")
      |> String.downcase

    ask(mobile, target, question)
  end

  def ask(mobile, nil, _question) do
    Mobile.send_scroll(mobile, "<p>Ask whom?</p>")
  end

  def ask(mobile, target, _question) when mobile == target do
    Mobile.send_scroll(mobile, "<p>Ask yourself?</p>")
  end

  def ask(mobile, target, question) do
    target = World.mobile(target)

    if target.questions |> Map.keys |> Enum.member?(question) do
      Mobile.execute_script(mobile, target.questions[question])
    else
      Mobile.send_scroll(mobile, "<p><span class='dark-green'>#{target.name} has nothing to tell you!</span></p>")
    end
  end

  defp find_mobile_in_room(mobile, string) do
    PubSub.subscribers("rooms:#{Mobile.room_id(mobile)}:mobiles")
    |> Systems.Match.one(:name_contains, string)
  end

end
