defmodule ApathyDrive.Commands.Turn do
  use ApathyDrive.Command
  alias ApathyDrive.{PubSub, Match}

  def keywords, do: ["turn"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Turn what?</p>")
  end
  def execute(mobile, arguments) do
    target = find_mobile_in_room(mobile, Enum.join(arguments, " "))
    turn(mobile, target)
  end

  def turn(mobile, nil) do
    Mobile.send_scroll(mobile, "<p>Turn what?</p>")
  end

  def turn(mobile, target) when mobile == target do
    Mobile.send_scroll(mobile, "<p>Turn yourself?</p>")
  end

  def turn(mobile, target) do
    data = Mobile.turn_data(mobile)

    case Mobile.turn(target, data) do
      {:ok, name, exp} ->
        Mobile.add_experience(mobile, -exp)
        Mobile.send_scroll(mobile, "<p>You turn #{name} to your side at a cost of #{exp} essence.</p>")
      {:error, :already_turned, name} ->
        Mobile.send_scroll(mobile, "<p><span class='dark-yellow'>#{Systems.Text.capitalize_first(name)} already serves you.</span></p>")
      {:error, :already_turned_by_other, name} ->
        Mobile.send_scroll(mobile, "<p><span class='dark-yellow'>#{Systems.Text.capitalize_first(name)} already serves another faction.</span></p>")
      {:error, :not_enough_essence, name, essence_required} ->
        Mobile.send_scroll(mobile, "<p><span class='dark-yellow'>#{Systems.Text.capitalize_first(name)} requires #{essence_required} to be turned, and you don't have enough.</span></p>")  
    end
  end

  defp find_mobile_in_room(mobile, string) do
    PubSub.subscribers("rooms:#{Mobile.room_id(mobile)}:mobiles")
    |> Match.one(:name_contains, string)
  end

end
