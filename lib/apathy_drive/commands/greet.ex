defmodule ApathyDrive.Commands.Greet do
  use ApathyDrive.Command

  def keywords, do: ["greet"]

  def execute(%Room{} = room, %Monster{monster_template_id: nil} = monster, _args) do
    Monster.body_required(monster)
    room
  end

  def execute(%Room{} = room, %Monster{} = monster, []) do
    Monster.send_scroll(monster, "<p>Greet whom?</p>")
    room
  end

  def execute(%Room{} = room, %Monster{} = monster, arguments) do
    query =
      arguments
      |> Enum.join(" ")
      |> String.downcase

    target = Room.find_monster_in_room(room, monster, query)
    greet(room, monster, target)
    room
  end

  def greet(%Room{}, %Monster{} = greeter, nil) do
    Monster.send_scroll(greeter, "<p>Greet whom?</p>")
  end

  def greet(%Room{}, %Monster{} = greeter, %Monster{} = target) when greeter == target do
    Monster.send_scroll(greeter, "<p>Greet yourself?</p>")
  end

  def greet(%Room{} = room, %Monster{} = greeter, %Monster{} = target) do
    room.monsters
    |> Enum.each(fn({_ref, monster}) ->
         cond do
           monster == greeter ->
             Monster.send_scroll(monster, "<p>You greet #{Monster.look_name(target)}.</p>")
             Monster.send_scroll(monster, "<p>#{target.greeting}</p>")
           monster == target ->
             Monster.send_scroll(monster, "<p>#{Monster.look_name(greeter)} greets you.</p>")
           true ->
             Monster.send_scroll(monster, "<p>#{Monster.look_name(greeter)} greets #{Monster.look_name(target)}.</p>")
         end
       end)
  end
end
