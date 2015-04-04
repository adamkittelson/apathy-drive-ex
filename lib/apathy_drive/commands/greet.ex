defmodule Commands.Greet do
  use ApathyDrive.Command
  alias ApathyDrive.PubSub

  def keywords, do: ["greet"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
  end

  def execute(%Monster{} = monster, arguments) do
    current_room = Monster.find_room(monster)

    if Enum.any? arguments do
      target = current_room |> find_monster_in_room(Enum.join(arguments, " "), monster)
      greet(monster, target, current_room)
    else
      Monster.send_scroll(monster, "<p>Greet whom?</p>")
    end
  end

  def greet(%Monster{} = monster, nil, %Room{}) do
    Monster.send_scroll(monster, "<p>Greet whom?</p>")
  end

  def greet(%Monster{} = monster, target, %Room{} = room) when is_pid(target) do
    greet(monster, Monster.value(target), room)
  end

  def greet(%Monster{} = monster, %Monster{} = target, %Room{}) when monster == target do
    Monster.send_scroll(monster, "<p>Greet yourself?</p>")
  end

  def greet(%Monster{} = monster, %Monster{} = target, %Room{} = room) do
    PubSub.broadcast "rooms:#{room.id}", {:greet, %{greeter: monster, greeted: target}}
    monster
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
