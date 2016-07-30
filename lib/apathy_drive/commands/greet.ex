defmodule ApathyDrive.Commands.Greet do
  use ApathyDrive.Command
  alias ApathyDrive.PubSub

  def keywords, do: ["greet"]

  def execute(%Room{} = room, %Mobile{monster_template_id: nil} = mobile, _args) do
    Mobile.body_required(mobile)
    room
  end

  def execute(%Room{} = room, %Mobile{} = mobile, []) do
    Mobile.send_scroll(mobile, "<p>Greet whom?</p>")
  end

  def execute(%Room{} = room, %Mobile{} = mobile, arguments) do
    query =
      arguments
      |> Enum.join(" ")
      |> String.downcase

    target = Room.find_mobile_in_room(room, mobile, query)
    greet(room, mobile, target)
    room
  end

  def greet(%Room{}, %Mobile{} = greeter, nil) do
    Mobile.send_scroll(greeter, "<p>Greet whom?</p>")
  end

  def greet(%Room{}, %Mobile{} = greeter, %Mobile{} = target) when greeter == target do
    Mobile.send_scroll(greeter, "<p>Greet yourself?</p>")
  end

  def greet(%Room{} = room, %Mobile{} = greeter, %Mobile{} = target) do
    room.mobiles
    |> Enum.each(fn({_ref, mobile}) ->
         cond do
           mobile == greeter ->
             Mobile.send_scroll(mobile, "<p>You greet #{Mobile.look_name(target)}.</p>")
             Mobile.send_scroll(mobile, "<p>#{target.greeting}</p>")
           mobile == target ->
             Mobile.send_scroll(mobile, "<p>#{Mobile.look_name(greeter)} greets you.</p>")
           true ->
             Mobile.send_scroll(mobile, "<p>#{Mobile.look_name(greeter)} greets #{Mobile.look_name(target)}.</p>")
         end
       end)
  end
end
