defmodule ApathyDrive.Commands.Ask do
  use ApathyDrive.Command

  def keywords, do: ["ask"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Ask whom?</p>")
  end
  def execute(mobile, [_]) do
    Mobile.send_scroll(mobile, "<p>Ask what?</p>")
  end
  def execute(mobile, arguments) do
    [target | question] = arguments

    question =
      question
      |> Enum.join(" ")
      |> String.downcase

    Mobile.ask(mobile, target, question)
  end

  def execute(%Mobile{room_id: room_id}, query, question) do
    room_id
    |> Room.find
    |> Room.ask(self(), query, question)
  end

  def execute(%Room{} = room, mobile, query, question) do
    target = Room.find_mobile_in_room(room, mobile, query)
    ask(mobile, target && target.pid, question)
  end

  def ask(mobile, nil, _question) do
    Mobile.send_scroll(mobile, "<p>Ask whom?</p>")
  end

  def ask(mobile, target, _question) when mobile == target do
    Mobile.send_scroll(mobile, "<p>Ask yourself?</p>")
  end

  def ask(mobile, target, question) do
    Mobile.answer(target, mobile, question)
  end

  def answer(%Mobile{} = target, mobile, question) do
    questions = MonsterTemplate.questions(target.monster_template_id)

    if questions |> Map.keys |> Enum.member?(question) do
      Mobile.execute_script(mobile, questions[question])
    else
      Mobile.send_scroll(mobile, "<p><span class='dark-green'>#{target.name} has nothing to tell you!</span></p>")
    end
  end

end
