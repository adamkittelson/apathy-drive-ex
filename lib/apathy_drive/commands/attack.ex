defmodule Commands.Attack do
  use ApathyDrive.Command
  alias ApathyDrive.PubSub

  def keywords, do: ["a", "attack", "k", "kill"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Attack whom?</p>")
  end
  def execute(mobile, arguments) do
    target = find_mobile_in_room(mobile, Enum.join(arguments, " "))
    attack(mobile, target)
  end

  def attack(mobile, nil) do
    Mobile.send_scroll(mobile, "<p>Attack whom?</p>")
  end

  def attack(mobile, target) when mobile == target do
    Mobile.send_scroll(mobile, "<p>Attack yourself?</p>")
  end

  def attack(mobile, target) do
    send(mobile, {:attack, target})
  end

  defp find_mobile_in_room(mobile, string) do
    PubSub.subscribers("rooms:#{Mobile.room_id(mobile)}:mobiles")
    |> Systems.Match.one(:name_contains, string)
  end

end
