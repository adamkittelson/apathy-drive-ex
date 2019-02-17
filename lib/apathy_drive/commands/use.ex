defmodule ApathyDrive.Commands.Use do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Item, ItemInstance, Match, Mobile, Repo, Room}

  def keywords, do: ["use", "light"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(character, "<p>Use what?</p>")
    room
  end

  def execute(%Room{} = room, %Character{} = character, arguments) do
    item_name = Enum.join(arguments, " ")

    character.inventory
    |> Match.one(:name_contains, item_name)
    |> case do
      nil ->
        Mobile.send_scroll(character, "<p>You don't have \"#{item_name}\".</p>")
        room

      %Item{type: "Light", instance_id: instance_id} = item ->
        ItemInstance
        |> Repo.get(instance_id)
        |> Ecto.Changeset.change(%{
          equipped: true
        })
        |> Repo.update!()

        Mobile.send_scroll(character, "<p>You lit the #{item.name}.</p>")

        Room.update_mobile(room, character.ref, fn char ->
          Character.load_items(char)
        end)
    end
  end

  def use_light_source(%Room{} = room, mobile_ref) do
    Room.update_mobile(room, mobile_ref, fn mobile ->
      if light = Enum.find(mobile.equipment, &(&1.type == "Light")) do
        if light.uses > 1 do
          ItemInstance
          |> Repo.get(light.instance_id)
          |> Ecto.Changeset.change(%{
            uses: light.uses - 1
          })
          |> Repo.update!()

          Character.load_items(mobile)
        else
          ItemInstance
          |> Repo.get(light.instance_id)
          |> Repo.delete!()

          Mobile.send_scroll(mobile, "<p>#{light.destruct_message}</p>")
          Character.load_items(mobile)
        end
      else
        room
      end
    end)
  end
end
