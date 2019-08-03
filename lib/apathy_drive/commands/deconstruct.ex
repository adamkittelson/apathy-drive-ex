defmodule ApathyDrive.Commands.Deconstruct do
  use ApathyDrive.Command

  alias ApathyDrive.{
    Character,
    CharacterMaterial,
    CraftingRecipe,
    Item,
    ItemInstance,
    Match,
    Material,
    Mobile,
    Repo
  }

  require Ecto.Query

  def keywords, do: ["decon", "deconstruct"]

  def execute(%Room{} = room, %Character{} = character, arguments) do
    item_name = Enum.join(arguments, " ")

    if String.trim(item_name) == "" do
      Mobile.send_scroll(
        character,
        "<p><span class='red'>Syntax: DECONSTRUCT {item}</span></p>"
      )

      room
    else
      deconstruct(room, character, item_name)
    end
  end

  def deconstruct(%Room{} = room, character, %Item{} = item) do
    if recipe = CraftingRecipe.for_item(item) do
      material = Repo.get(Material, recipe.material_id)
      amount = 0..recipe.material_amount |> Enum.random()

      if amount > 0 do
        Mobile.send_scroll(
          character,
          "<p>You deconstruct #{Item.colored_name(item)} and receive #{amount} #{material.name}.</p>"
        )

        Room.update_mobile(room, character.ref, fn character ->
          case character.materials[material.name] do
            %CharacterMaterial{amount: current} = cm ->
              cm
              |> Ecto.Changeset.change(%{
                amount: current + amount
              })
              |> Repo.update!()

            nil ->
              %CharacterMaterial{
                character_id: character.id,
                material_id: material.id,
                amount: amount
              }
              |> Repo.insert!()
          end

          ItemInstance
          |> Repo.get(item.instance_id)
          |> Repo.delete!()

          character
          |> Character.load_materials()
          |> Character.load_items()
        end)
      else
        Mobile.send_scroll(
          character,
          "<p>You deconstruct #{Item.colored_name(item)} but fail to extract any materials.</p>"
        )

        room
      end
    else
      Mobile.send_scroll(character, "<p>You cannot deconstruct #{Item.colored_name(item)}.</p>")
      room
    end
  end

  def deconstruct(%Room{} = room, character, item_name) do
    character.inventory
    |> Match.all(:name_contains, item_name)
    |> case do
      nil ->
        Mobile.send_scroll(character, "<p>You don't have \"#{item_name}\"!</p>")
        room

      %Item{} = item ->
        deconstruct(room, character, item)

      matches ->
        if Enum.all?(matches, &(&1.name == List.first(matches).name)) do
          deconstruct(room, character, List.first(matches))
        else
          Mobile.send_scroll(
            character,
            "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
          )

          Enum.each(matches, fn match ->
            Mobile.send_scroll(character, "<p>-- #{Item.colored_name(match)}</p>")
          end)

          room
        end
    end
  end
end
