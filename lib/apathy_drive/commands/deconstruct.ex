defmodule ApathyDrive.Commands.Deconstruct do
  use ApathyDrive.Command

  alias ApathyDrive.{
    Character,
    CharacterMaterial,
    CharacterStyle,
    CraftingRecipe,
    Enchantment,
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

        learn_style(character, item)

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

          item_instance =
            ItemInstance
            |> Repo.get(item.instance_id)
            |> Map.put(:item, item)

          Repo.delete!(item_instance)

          character
          |> Character.load_materials()
          |> Character.load_items()
          |> Enchantment.add_enchantment_exp(%Enchantment{items_instances: item_instance})
        end)
      else
        Mobile.send_scroll(
          character,
          "<p>You deconstruct #{Item.colored_name(item)} but fail to extract any materials.</p>"
        )

        learn_style(character, item)

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

  defp learn_style(%Character{} = character, %Item{} = item) do
    %CharacterStyle{}
    |> Ecto.Changeset.change(character_id: character.id, item_id: item.id)
    |> Ecto.Changeset.unique_constraint(:character_id,
      name: :characters_styles_character_id_item_id_index
    )
    |> Repo.insert()
    |> case do
      {:ok, _style} ->
        Mobile.send_scroll(character, "<p>You learn how to craft #{Item.colored_name(item)}!")

      _ ->
        :noop
    end
  end
end
