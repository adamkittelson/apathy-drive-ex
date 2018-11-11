defmodule ApathyDrive.Commands.Read do
  use ApathyDrive.Command

  alias ApathyDrive.{
    Character,
    CharacterAbility,
    Item,
    ItemInstance,
    Match,
    Repo,
    Room
  }

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
        ability = item.traits["Learn"]

        cond do
          Item.too_powerful_for_character?(character, item) ->
            message = "<p>You do not meet the attribute requirements to read #{item.name}.</p>"
            Mobile.send_scroll(character, message)

            room

          already_active?(character, ability) ->
            message = "<p>You already have #{ability.name} activated.</p>"
            Mobile.send_scroll(character, message)

            room

          too_many_active_abilities?(character) ->
            message = "<p>You must forget an ability before you can learn any more abilities.</p>"

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

  defp already_active?(character, ability) do
    !!Repo.get_by(CharacterAbility, character_id: character.id, ability_id: ability.id)
  end

  defp too_many_active_abilities?(character) do
    map_size(character.abilities) >= Character.max_active_abilities(character)
  end

  defp learn_ability(room, character, scroll, ability) do
    Room.update_mobile(room, character.ref, fn character ->
      message =
        "<p>As you read the #{scroll.name} it crumbles to dust. You now have the knowledge of #{
          ability.name
        }!</p>"

      Mobile.send_scroll(character, message)

      %CharacterAbility{character_id: character.id, ability_id: ability.id}
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
