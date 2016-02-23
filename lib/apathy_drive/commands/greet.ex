defmodule Commands.Greet do
  use ApathyDrive.Command
  alias ApathyDrive.{PubSub, World, Match}

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

  def greet(mobile, target) do
    mob = World.mobile(target)
    target_greeting = 
      mob.greeting || "You greet #{mob.name}."

    Mobile.send_scroll(target, "<p>#{Mobile.name(mobile)} greets you.</p>")
    Mobile.send_scroll(mobile, "<p>#{target_greeting}</p>")

    PubSub.subscribers("rooms:#{Mobile.room_id(mobile)}:mobiles", [mobile, target])
    |> Enum.each(&(Mobile.send_scroll(&1, "<p>#{Mobile.name(mobile)} greets #{Mobile.name(target)}.</p>")))
  end

  defp find_mobile_in_room(mobile, string) do
    PubSub.subscribers("rooms:#{Mobile.room_id(mobile)}:mobiles")
    |> Match.one(:name_contains, string)
  end

end
