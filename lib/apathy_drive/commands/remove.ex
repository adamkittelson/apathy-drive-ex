defmodule ApathyDrive.Commands.Remove do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Item, ItemInstance, Match, Mobile, Repo}

  def keywords, do: ["remove", "unequip", "unwield", "rem"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(character, "<p>Remove what?</p>")
    room
  end

  def execute(%Room{} = room, %Character{} = character, arguments) do
    item_name = Enum.join(arguments, " ")

    character.equipment
    |> Match.one(:name_contains, item_name)
    |> case do
      nil ->
        Mobile.send_scroll(character, "<p>You don't have \"#{item_name}\" equipped.</p>")
        room

      %{} = item_to_remove ->
        ItemInstance
        |> Repo.get(item_to_remove.instance_id)
        |> Ecto.Changeset.change(%{equipped: false})
        |> Repo.update!()

        room =
          Room.update_mobile(room, character.ref, fn char ->
            character =
              char
              |> Character.load_items()

            if item_to_remove.type == "Light" do
              Mobile.send_scroll(
                char,
                "<p>You remove the #{Item.colored_name(item_to_remove)} and extinguish it.</p>"
              )
            else
              Mobile.send_scroll(char, "<p>You remove #{Item.colored_name(item_to_remove)}.</p>")
            end

            send(
              character.socket,
              {:update_character,
               %{
                 room_id: room.id,
                 power: Mobile.power_at_level(character, character.level),
                 level: character.level
               }}
            )

            character
          end)

        Room.update_hp_bar(room, character.ref)
        Room.update_mana_bar(room, character.ref)
        room
    end
  end
end
