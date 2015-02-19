defmodule Commands.Ask do
  use ApathyDrive.Command
  alias Phoenix.PubSub

  def keywords, do: ["ask"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
  end

  def execute(%Monster{} = monster, []),          do: Monster.send_scroll(monster, "<p>Ask whom?</p>")
  def execute(%Monster{} = monster, [_question]), do: Monster.send_scroll(monster, "<p>Ask what?</p>")

  def execute(%Monster{} = monster, arguments) do
    current_room = Monster.find_room(monster)

    [target | question] = arguments

    target = current_room |> find_monster_in_room(target, monster)

    ask(monster, target, Enum.join(question, " "))
  end

  def ask(%Monster{} = monster, nil, _question) do
    send_message(monster, "scroll", "<p>Ask whom?</p>")
  end

  def ask(%Monster{} = monster, target, question) when is_pid(target) do
    ask(monster, Monster.value(target), question)
  end

  def ask(%Monster{} = monster, %Monster{} = target, question) do
    if monster.questions |> Map.keys |> Enum.member?(question) do
      Systems.Script.execute(monster.questions[question], monster)
      monster
    else
      Monster.send_scroll(monster, "<p><span class='dark-green'>#{target.name} has nothing to tell you!</span></p>")
    end
  end

  defp find_monster_in_room(%Room{} = room, string, %Monster{pid: pid} = monster) do
    PubSub.subscribers("rooms:#{room.id}:monsters")
    |> Enum.map(fn(monster_pid) ->
         if monster_pid == pid do
           monster
         else
           monster_pid
         end
       end)
    |> Systems.Match.one(:name_contains, string)
  end

end
