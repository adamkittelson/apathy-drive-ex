defmodule ApathyDrive.Commands.Ask do
  use ApathyDrive.Command
  alias ApathyDrive.MonsterTemplate

  def keywords, do: ["ask"]

  def execute(%Room{} = room, %Monster{monster_template_id: nil} = monster, _args) do
    Monster.body_required(monster)
    room
  end

  def execute(%Room{} = room, %Monster{} = monster, []) do
    Monster.send_scroll(monster, "<p>Ask whom?</p>")
    room
  end

  def execute(%Room{} = room, %Monster{} = monster, [_]) do
    Monster.send_scroll(monster, "<p>Ask what?</p>")
    room
  end

  def execute(%Room{} = room, %Monster{} = monster, arguments) do
    [target | question] = arguments

    question =
      question
      |> Enum.join(" ")
      |> String.downcase

    target = Room.find_monster_in_room(room, monster, target)
    ask(room, monster, target, question)
  end

  def ask(%Room{} = room, %Monster{} = monster, nil, _question) do
    Monster.send_scroll(monster, "<p>Ask whom?</p>")
    room
  end

  def ask(%Room{} = room, %Monster{} = monster, %Monster{} = target, _question) when monster == target do
    Monster.send_scroll(monster, "<p>Ask yourself?</p>")
    room
  end

  def ask(%Room{} = room, %Monster{} = monster, %Monster{} = target, question) do
    questions = MonsterTemplate.questions(target.monster_template_id)

    if questions |> Map.keys |> Enum.member?(question) do
      ApathyDrive.Script.execute(room, monster, questions[question])
    else
      Monster.send_scroll(monster, "<p><span class='dark-green'>#{target.name} has nothing to tell you!</span></p>")
      room
    end
  end

end
