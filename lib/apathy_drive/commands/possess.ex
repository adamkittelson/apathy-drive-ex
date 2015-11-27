defmodule Commands.Possess do
  use ApathyDrive.Command
  alias ApathyDrive.PubSub

  def keywords, do: ["possess"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Possess what?</p>")
  end
  def execute(mobile, arguments) do
    case Mobile.able_to_possess?(mobile) do
      :ok ->
        target = find_mobile_in_room(mobile, Enum.join(arguments, " "))
        possess(mobile, target)
      {:error, error} ->
        Mobile.send_scroll(mobile, "<p>#{error}</p>")
    end
  end

  def possess(mobile, nil) do
    Mobile.send_scroll(mobile, "<p>Possess what?</p>")
  end

  def possess(mobile, target) when mobile == target do
    Mobile.send_scroll(mobile, "<p>Possess yourself?</p>")
  end

  def possess(mobile, target) do
    case Mobile.possess(target, Mobile.spirit_id(mobile), self) do
      :ok ->
        Process.unlink(mobile)
        Process.exit(mobile, :kill)
      {:error, reason} ->
        Mobile.send_scroll(mobile, "<p>#{reason}</p>")
    end
  end

  defp find_mobile_in_room(mobile, string) do
    PubSub.subscribers("rooms:#{Mobile.room_id(mobile)}:mobiles")
    |> Systems.Match.one(:name_contains, string)
  end

end
