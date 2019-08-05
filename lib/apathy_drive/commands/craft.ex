defmodule ApathyDrive.Commands.Craft do
  use ApathyDrive.Command

  alias ApathyDrive.{
    Character,
    CharacterStyle,
    CraftingRecipe,
    Item,
    ItemInstance,
    ItemTrait,
    Match,
    Mobile,
    Repo
  }

  require Ecto.Query

  def keywords, do: ["craft"]

  def execute(%Room{} = room, %Character{} = character, []) do
    display_crafts(character)
    room
  end

  def execute(%Room{} = room, %Character{} = character, ["list"]) do
    display_crafts(character)
    room
  end

  def execute(%Room{} = room, %Character{} = character, ["level", level | item]) do
    case Integer.parse(level) do
      {level, ""} ->
        item_name = Enum.join(item, " ")

        character
        |> CharacterStyle.for_character()
        |> Ecto.Query.preload(:item)
        |> Repo.all()
        |> Enum.map(& &1.item)
        |> Match.all(:name_starts_with, item_name)
        |> case do
          nil ->
            Mobile.send_scroll(character, "<p>You do not know how to craft #{item_name}.</p>")

          %Item{} = item ->
            recipe =
              item
              |> Map.put(:level, level)
              |> CraftingRecipe.for_item()

            if item.weight <=
                 Character.max_encumbrance(character) - Character.encumbrance(character) do
              %ItemInstance{
                item_id: item.id,
                level: level,
                character_id: character.id,
                equipped: false,
                hidden: false
              }
              |> Repo.insert!()

              room
              |> Room.update_mobile(character.ref, fn char ->
                char
                |> Character.load_items()
                |> Mobile.send_scroll("<p>You craft a #{Item.colored_name(item)}.</p>")
              end)
            else
              Mobile.send_scroll(character, "<p>#{Item.colored_name(item)} is too heavy.</p>")
              room
            end

          matches ->
            Mobile.send_scroll(
              character,
              "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
            )

            Enum.each(matches, fn match ->
              Mobile.send_scroll(character, "<p>-- #{Item.colored_name(match)}</p>")
            end)

            room
        end

      _ ->
        Mobile.send_scroll(
          character,
          "<p><span class='red'>Syntax: craft level {level} {item}</span></p>"
        )

        room
    end
  end

  def execute(%Room{} = room, %Character{} = character, _arguments) do
    Mobile.send_scroll(
      character,
      "<p><span class='red'>Syntax: craft level {level} {item}</span></p>"
    )

    room
  end

  def display_crafts(%Character{} = character) do
    Mobile.send_scroll(
      character,
      "<p><span class='white'>You know how to craft following items:</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-magenta'>Skill         Material      Worn On</span></p>"
    )

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
          |> Enum.map(&[&1.item.type, &1.item.armour_type, &1.item.worn_on, &1])
          |> Enum.group_by(&Enum.slice(&1, 0..2))
          |> Enum.each(fn {[_type, subtype, worn_on], styles} ->
            styles =
              styles
              |> Enum.map(&List.last/1)
              |> Enum.map(fn %{item: item} ->
                Map.put(item, :traits, ItemTrait.load_traits(item.id))
              end)
              |> Enum.sort_by(& &1.traits["Quality"])
              |> Enum.map(&Item.colored_name(&1))

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
