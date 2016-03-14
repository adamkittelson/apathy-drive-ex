defmodule Commands.Greet do
  use ApathyDrive.Command
  alias ApathyDrive.{PubSub, Match}

  def keywords, do: ["greet"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Greet whom?</p>")
  end
  def execute(mobile, arguments) do
    target = find_mobile_in_room(mobile, Enum.join(arguments, " "))
    greet(mobile, target)
  end

  def greet(mobile, nil) do
    Mobile.send_scroll(mobile, "<p>Greet whom?</p>")
  end

  def greet(mobile, target) when mobile == target do
    Mobile.send_scroll(mobile, "<p>Greet yourself?</p>")
  end

  def greet(%Mobile{} = target, %{name: name, pid: greeter}) do
    Mobile.send_scroll(greeter, "<p>You greet #{target.name}.</p>")
    Mobile.send_scroll(target, "<p>#{name} greets you.</p>")

    PubSub.subscribers("rooms:#{target.room_id}:mobiles", [greeter, self])
    |> Enum.each(&(Mobile.send_scroll(&1, "<p>#{name} greets #{target.name}.</p>")))

    unless target.spirit do
      Mobile.send_scroll(greeter, target.greeting)
    end
  end

  def greet(%Mobile{} = mobile, target) do
    Mobile.greet(target, %{name: mobile.name, pid: self()})
  end

  def greet(mobile, target) do
    Mobile.greet(mobile, target)
  end

  defp find_mobile_in_room(mobile, string) do
    PubSub.subscribers("rooms:#{Mobile.room_id(mobile)}:mobiles")
    |> Match.one(:name_contains, string)
  end

end
