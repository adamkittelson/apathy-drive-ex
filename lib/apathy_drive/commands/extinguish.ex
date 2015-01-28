defmodule Commands.Extinguish do
  use ApathyDrive.Command

  def keywords, do: ["extinguish", "douse"]

  def execute(spirit, nil, _arguments) do
    send_message(spirit, "scroll", "<p>You need a body to do that.</p>")
  end

  def execute(spirit, monster, arguments) do
    current_room = Parent.of(monster)

    if Enum.any? arguments do
      cond do
        target = find_item_on_monster(monster, Enum.join(arguments, " ")) ->
          case Systems.Light.extinguish(target) do
            :not_a_light ->
              send_message(monster, "scroll", "<p>You can't extinguish a #{Components.Name.value(target)}!</p>")
            :not_lit ->
              send_message(monster, "scroll", "<p>The #{Components.Name.value(target)} isn't lit!</p>")
            :always_lit ->
              send_message(monster, "scroll", "<p>The #{Components.Name.value(target)}'s light can not be extinguished!</p>")
            _ ->
              send_message(monster, "scroll", "<p>You extinguish the #{Components.Name.value(target)}.</p>")
          end
      true ->
        send_message(monster, "scroll", "<p>You aren't carrying that.</p>")
      end
    else
      send_message(monster, "scroll", "<p>Extinguish what?</p>")
    end
  end

  defp find_item_on_monster(monster, string) do
    (Systems.Limbs.equipped_items(monster) ++ Components.Items.get_items(monster))
    |> Systems.Match.one(:name_contains, string)
  end

end
