defmodule ApathyDrive.Commands.Inventory do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Currency, Item, Mobile}

  def keywords, do: ["i", "inv", "inventory"]

  def execute(
        %Room{} = room,
        %Character{equipment: equipment, inventory: inventory} = character,
        _args
      ) do
    if equipment |> Enum.any?() do
      Mobile.send_scroll(
        character,
        "<p><span class='dark-yellow'>You are equipped with:</span></p><br>"
      )

      equipment
      |> Enum.each(fn item ->
        Mobile.send_scroll(
          character,
          "<p><span class='dark-green'>#{Item.colored_name(item, pad_trailing: 30)}</span><span class='dark-cyan'>(#{
            item.worn_on
          })</span></p>"
        )
      end)

      Mobile.send_scroll(character, "<br>")
    end

    item_names = inventory |> Enum.map(&Item.colored_name(&1))

    item_names = Currency.to_list(character) ++ item_names

    if item_names |> Enum.count() > 0 do
      Mobile.send_scroll(character, "<p>You are carrying #{item_names |> to_sentence()}</p>")
    else
      Mobile.send_scroll(character, "<p>You are carrying nothing.</p>")
    end

    Mobile.send_scroll(
      character,
      "<p><span class='dark-green'>Wealth:</span> <span class='dark-cyan'>#{
        Currency.wealth(character)
      } copper farthings</span></p>"
    )

    room
  end

  def to_sentence(list) do
    case length(list) do
      0 ->
        ""

      1 ->
        List.first(list)

      2 ->
        Enum.join(list, " and ")

      _ ->
        {last, list} = List.pop_at(list, -1)
        Enum.join(list, ", ") <> " and " <> last
    end
  end
end
