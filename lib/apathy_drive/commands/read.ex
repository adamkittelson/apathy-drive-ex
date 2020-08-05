defmodule ApathyDrive.Commands.Read do
  use ApathyDrive.Command

  alias ApathyDrive.{
    Ability,
    Character,
    CharacterAbility,
    ClassAbility,
    Item,
    ItemInstance,
    Match,
    Repo,
    Room
  }

  require Ecto.Query

  def keywords, do: ["read"]

  def execute(%Room{} = room, %Character{} = character, []) do
    message = "<p><span class='red'>Syntax: READ {scroll}</span></p>"
    Mobile.send_scroll(character, message)

    room
  end

  def execute(%Room{} = room, %Character{} = character, args) do
    scroll = Enum.join(args, " ")

    character.inventory
    |> Enum.filter(&(&1.type == "Scroll"))
    |> Match.one(:name_contains, scroll)
    |> case do
      %Item{} = item ->
        ability = Systems.Effect.effect_bonus(item, "Learn")

        cond do
          Item.too_powerful_for_character?(character, item) ->
            message = "<p>You do not meet the requirements to read #{item.name}.</p>"
            Mobile.send_scroll(character, message)

            room

          already_learned?(character, ability) ->
            message = "<p>You already know #{ability.name}.</p>"
            Mobile.send_scroll(character, message)

            room

          wrong_class?(character, ability) ->
            message = "<p>You cannot learn #{ability.name}.</p>"

            Mobile.send_scroll(character, message)
            room

          !Ability.appropriate_alignment?(ability, character) ->
            message = "<p>You don't have the disposition to learn #{ability.name}.</p>"

            Mobile.send_scroll(character, message)
            room

          :else ->
            learn_ability(room, character, item, ability)
        end

      nil ->
        message = "<p><span class='red'>You cannot read that!</span></p>"
        Mobile.send_scroll(character, message)

        room
    end
  end

  def already_learned?(_character, nil), do: false

  def already_learned?(character, ability) do
    !!Repo.get_by(CharacterAbility, character_id: character.id, ability_id: ability.id)
  end

  def wrong_class?(_character, nil), do: true

  def wrong_class?(character, ability) do
    class_ids =
      character.classes
      |> Enum.map(& &1.class_id)
      |> Enum.into(MapSet.new())

    ability_classes =
      ClassAbility
      |> Ecto.Query.where([ca], ca.ability_id == ^ability.id)
      |> Ecto.Query.select([:class_id])
      |> Repo.all()
      |> Enum.map(& &1.class_id)
      |> Enum.into(MapSet.new())

    IO.puts("class_ids: #{inspect(class_ids)}, ability_classes: #{inspect(ability_classes)}")

    !(Enum.any?(class_ids) and Enum.any?(MapSet.intersection(class_ids, ability_classes)))
  end

  defp learn_ability(room, character, scroll, ability) do
    Room.update_mobile(room, character.ref, fn _room, character ->
      Mobile.send_scroll(
        character,
        "<p>You read #{scroll.name} and learn the spell #{ability.name}.</span></p>"
      )

      Mobile.send_scroll(character, "<p>Its magic used, the #{scroll.name} disintegrates.</p>")

      %CharacterAbility{character_id: character.id, ability_id: ability.id, learned: true}
      |> Repo.insert!()

      ItemInstance
      |> Repo.get!(scroll.instance_id)
      |> Repo.delete!()

      character
      |> Character.load_abilities()
      |> Character.load_items()
    end)
  end
end
