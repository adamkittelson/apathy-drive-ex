defmodule ApathyDrive.Commands.Possess do
  use ApathyDrive.Command
  alias ApathyDrive.{PubSub, World, Match}

  def keywords, do: ["possess"]

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Possess what?</p>")
  end
  def execute(mobile, arguments) do
    case World.mobile(mobile) do
      %Mobile{monster_template_id: nil} ->
        target = find_mobile_in_room(mobile, Enum.join(arguments, " "))
        possess(mobile, target)
      %Mobile{monster_template_id: _, name: name} ->
        Mobile.send_scroll(mobile, "<p>You are already possessing #{name}.</p>")
    end
  end

  def possess(mobile, nil) do
    Mobile.send_scroll(mobile, "<p>Possess what?</p>")
  end

  def possess(mobile, target) when mobile == target do
    Mobile.send_scroll(mobile, "<p>Possess yourself?</p>")
  end

  def possess(mobile, target) do
    mob = World.mobile(mobile)

    case Mobile.possess(target, mob.spirit.id, mob.spirit.class.name, self) do
      :ok ->
        Mobile.remove_effects(mobile)

        Process.unlink(mobile)
        Process.exit(mobile, :kill)
      {:error, reason} ->
        Mobile.send_scroll(mobile, "<p>#{reason}</p>")
    end
  end

  defp find_mobile_in_room(mobile, string) do
    PubSub.subscribers("rooms:#{Mobile.room_id(mobile)}:mobiles")
    |> Match.one(:name_contains, string)
  end

end
