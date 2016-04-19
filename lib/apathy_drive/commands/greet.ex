defmodule ApathyDrive.Commands.Greet do
  use ApathyDrive.Command
  alias ApathyDrive.PubSub

  def keywords, do: ["greet"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Greet whom?</p>")
  end

  def execute(mobile, arguments) when is_pid(mobile) do
    query =
      arguments
      |> Enum.join(" ")
      |> String.downcase

    Mobile.greet(mobile, query)
  end

  def execute(%Mobile{room_id: room_id} = mobile, query) do
    room_id
    |> Room.find
    |> Room.greet(%{name: Mobile.look_name(mobile), pid: self()}, query)
  end

  def execute(%Room{} = room, %{name: _name, pid: pid} = mobile, query) do
    IO.puts "query: #{inspect query}"
    target = Room.find_mobile_in_room(room, pid, query)
    greet(mobile, target && target.pid)
  end

  def greet(%Mobile{} = target, %{name: name, pid: greeter}) do
    Mobile.send_scroll(greeter, "<p>You greet #{Mobile.look_name(target)}.</p>")
    Mobile.send_scroll(target, "<p>#{name} greets you.</p>")

    PubSub.subscribers("rooms:#{target.room_id}:mobiles", [greeter, self()])
    |> Enum.each(&(Mobile.send_scroll(&1, "<p>#{name} greets #{Mobile.look_name(target)}.</p>")))

    unless target.spirit do
      Mobile.send_scroll(greeter, "<p>#{target.greeting}</p>")
    end
  end

  def greet(%{name: _name, pid: mobile}, nil) do
    Mobile.send_scroll(mobile, "<p>Greet whom?</p>")
  end

  def greet(%{pid: mobile}, target) when mobile == target do
    Mobile.send_scroll(mobile, "<p>Greet yourself?</p>")
  end

  def greet(%{name: _name, pid: _pid} = mobile, target) do
    Mobile.greet(target, mobile)
  end

end
