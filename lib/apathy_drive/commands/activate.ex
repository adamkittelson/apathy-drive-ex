defmodule ApathyDrive.Commands.Activate do
  use ApathyDrive.Command
  alias ApathyDrive.{Ability, Character, CharacterAbility, ClassAbility, Match, Repo, Room}
  require Ecto.Query

  def keywords, do: ["activate"]

  def execute(%Room{} = room, %Character{} = character, []) do
    message = "<p><span class='red'>Syntax: ACTIVATE {ability}</span></p>"
    Mobile.send_scroll(character, message)

    room
  end

  def execute(%Room{} = room, %Character{} = character, args) do
    ability = Enum.join(args, " ")

    character.class_id
    |> ClassAbility.abilities_at_level(character.level)
    |> Enum.map(& &1.ability)
    |> Match.one(:name_contains, ability)
    |> case do
      %Ability{} = ability ->
        cond do
          already_active?(character, ability) ->
            message = "<p>#{ability.name} is already active.</p>"
            Mobile.send_scroll(character, message)

            room

          too_many_active_abilities?(character) ->
            message =
              "<p>You must forget an ability before you can activate any more abilities.</p>"

            Mobile.send_scroll(character, message)

            room

          :else ->
            activate_ability(room, character, ability)
        end

      nil ->
        message = "<p><span class='red'>Syntax: ACTIVATE {ability}</span></p>"
        Mobile.send_scroll(character, message)

        room
    end
  end

  defp already_active?(character, ability) do
    !!Repo.get_by(CharacterAbility, character_id: character.id, ability_id: ability.id)
  end

  defp too_many_active_abilities?(%{id: id} = character) do
    count =
      ApathyDrive.CharacterAbility
      |> Ecto.Query.where([ca], ca.character_id == ^id)
      |> Ecto.Query.select([ca], count(ca.id))
      |> Repo.one()

    count >= Character.max_active_abilities(character)
  end

  defp activate_ability(room, character, ability) do
    Room.update_mobile(room, character.ref, fn character ->
      message = "<p>Activating #{ability.name}!</p>"
      Mobile.send_scroll(character, message)

      %CharacterAbility{character_id: character.id, ability_id: ability.id}
      |> Repo.insert!()

      Character.load_abilities(character)
    end)
  end
end
