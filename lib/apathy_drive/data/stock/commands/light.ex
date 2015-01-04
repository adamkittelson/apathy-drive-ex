defmodule Commands.Light do
  use Systems.Command

  def keywords, do: ["light"]

  def execute(spirit, nil, _arguments) do
    send_message(spirit, "scroll", "<p>You need a body to do that.</p>")
  end

  def execute(spirit, monster, arguments) do
    current_room = Parent.of(monster)

    if Enum.any? arguments do
      cond do
        target = find_item_on_monster(monster, Enum.join(arguments, " ")) ->
          case Systems.Light.light(target) do
            :not_a_light ->
              send_message(monster, "scroll", "<p>You can't light a #{Components.Name.value(target)}!</p>")
            :already_lit ->
              send_message(monster, "scroll", "<p>The #{Components.Name.value(target)} is already lit!</p>")
            _ ->
              send_message(monster, "scroll", "<p>You light the #{Components.Name.value(target)}.</p>")
          end
      true ->
        send_message(monster, "scroll", "<p>You aren't carrying that.</p>")
      end
    else
      send_message(monster, "scroll", "<p>Light what?</p>")
    end
  end

  defp find_item_on_monster(monster, string) do
    (Systems.Limbs.equipped_items(monster) ++ Components.Items.get_items(monster))
    |> Systems.Match.one(:name_contains, string)
  end

end
