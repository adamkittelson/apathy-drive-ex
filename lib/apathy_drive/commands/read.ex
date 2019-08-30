defmodule ApathyDrive.Commands.Read do
  use ApathyDrive.Command

  alias ApathyDrive.{
    Character,
    CharacterAbility,
    ClassAbility,
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
            message = "<p>You do not meet the requirements to read #{item.name}.</p>"
            Mobile.send_scroll(character, message)

            room

          already_learned?(character, ability) ->
            message = "<p>You already know #{ability.name}.</p>"
            Mobile.send_scroll(character, message)

            room

          wrong_class?(character, ability) ->
            message = "<p>Your class cannot learn #{ability.name}.</p>"

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

  def wrong_class?(_character, nil), do: true

  def wrong_class?(character, ability) do
    !Repo.get_by(ClassAbility, class_id: character.class_id, ability_id: ability.id)
  end

  def already_learned?(_character, nil), do: false

  def already_learned?(character, ability) do
    !!Repo.get_by(CharacterAbility, character_id: character.id, ability_id: ability.id)
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
