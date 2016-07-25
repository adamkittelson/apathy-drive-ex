defmodule ApathyDrive.Commands.Ask do
  use ApathyDrive.Command
  alias ApathyDrive.MonsterTemplate

  def keywords, do: ["ask"]

  def execute(%Room{} = room, %Mobile{monster_template_id: nil} = mobile, _args) do
    Mobile.body_required(mobile)
    room
  end

  def execute(%Room{} = room, %Mobile{} = mobile, []) do
    Mobile.send_scroll(mobile, "<p>Ask whom?</p>")
    room
  end

  def execute(%Room{} = room, %Mobile{} = mobile, [_]) do
    Mobile.send_scroll(mobile, "<p>Ask what?</p>")
    room
  end

  def execute(%Room{} = room, %Mobile{} = mobile, arguments) do
    [target | question] = arguments

    question =
      question
      |> Enum.join(" ")
      |> String.downcase

    target = Room.find_mobile_in_room(room, mobile, target)
    ask(room, mobile, target, question)
  end

  def ask(%Room{} = room, %Mobile{} = mobile, nil, _question) do
    Mobile.send_scroll(mobile, "<p>Ask whom?</p>")
    room
  end

  def ask(%Room{} = room, %Mobile{} = mobile, %Mobile{} = target, _question) when mobile == target do
    Mobile.send_scroll(mobile, "<p>Ask yourself?</p>")
    room
  end

  def ask(%Room{} = room, %Mobile{} = mobile, %Mobile{} = target, question) do
    questions = MonsterTemplate.questions(target.monster_template_id)

    if questions |> Map.keys |> Enum.member?(question) do
      ApathyDrive.Script.execute(room, mobile, questions[question])
    else
      Mobile.send_scroll(mobile, "<p><span class='dark-green'>#{target.name} has nothing to tell you!</span></p>")
      room
    end
  end

end
