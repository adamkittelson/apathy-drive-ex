defmodule ApathyDrive.Commands.Craft do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, CharacterStyle, CraftingRecipe, Item, Mobile, Repo}
  require Ecto.Query

  def keywords, do: ["craft"]

  def execute(%Room{} = room, %Character{} = character, _arguments) do
    Mobile.send_scroll(
      character,
      "<p><span class='white'>You know how to craft following items:</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-magenta'>Skill         Material      Worn On</span></p>"
    )

    display_crafts(character)
    room
  end

  def display_crafts(%Character{} = character) do
    styles =
      character
      |> CharacterStyle.for_character()
      |> Repo.all()

    character.skills
    |> Enum.map(fn {skill_name, %{skill_id: id}} ->
      Enum.map(CraftingRecipe.types_for_skill(id), fn columns ->
        [skill_name | columns]
      end)
    end)
    |> Enum.reject(&Enum.empty?/1)
    |> Enum.each(fn recipe ->
      case recipe |> List.flatten() |> Enum.reject(&is_nil/1) do
        [skill, "Armour", subtype, worn_on] ->
          styles
          |> Enum.filter(
            &(&1.item.type == "Armour" and &1.item.armour_type == subtype and
                &1.item.worn_on == worn_on)
          )
          |> Enum.map(
            &[&1.item.type, &1.item.armour_type, &1.item.worn_on, Item.colored_name(&1.item)]
          )
          |> Enum.group_by(&Enum.slice(&1, 0..2))
          |> Enum.each(fn {[_type, subtype, worn_on], styles} ->
            styles = Enum.map(styles, &List.last/1)

            Mobile.send_scroll(
              character,
              "<p><span class='dark-cyan'>#{String.pad_trailing(skill, 13)} #{
                String.pad_trailing(subtype, 13)
              } #{String.pad_trailing(worn_on, 13)}</span></p>"
            )

            Mobile.send_scroll(
              character,
              "<p>  #{ApathyDrive.Commands.Inventory.to_sentence(styles)}</p>"
            )
          end)

        [skill, "Weapon", subtype, worn_on] ->
          styles
          |> Enum.filter(
            &(&1.item.type == "Weapon" and &1.item.weapon_type == subtype and
                &1.item.worn_on == worn_on)
          )
          |> Enum.map(
            &[&1.item.type, &1.item.weapon_type, &1.item.worn_on, Item.colored_name(&1.item)]
          )
          |> Enum.group_by(&Enum.slice(&1, 0..2))
          |> Enum.each(fn {[_type, subtype, worn_on], styles} ->
            styles = Enum.map(styles, &List.last/1)

            Mobile.send_scroll(
              character,
              "<p><span class='dark-cyan'>#{String.pad_trailing(skill, 13)} #{
                String.pad_trailing(subtype, 13)
              } #{String.pad_trailing(worn_on, 13)}</span></p>"
            )

            Mobile.send_scroll(
              character,
              "<p>  #{ApathyDrive.Commands.Inventory.to_sentence(styles)}</p>"
            )
          end)
      end
    end)
  end
end
