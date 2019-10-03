defmodule ApathyDrive.Commands.Forget do
  use ApathyDrive.Command
  alias ApathyDrive.{Ability, Character, CharacterAbility, Match, Repo, Room}
  require Ecto.Query

  def keywords, do: ["deactivate"]

  def execute(%Room{} = room, %Character{} = character, []) do
    message = "<p><span class='red'>Syntax: FORGET {ability}</span></p>"
    Mobile.send_scroll(character, message)

    room
  end

  def execute(%Room{} = room, %Character{id: id} = character, args) do
    ability = Enum.join(args, " ")

    ApathyDrive.CharacterAbility
    |> Ecto.Query.where([ca], ca.character_id == ^id)
    |> Ecto.Query.preload([:ability])
    |> Repo.all()
    |> Enum.map(& &1.ability)
    |> Match.one(:name_contains, ability)
    |> case do
      %Ability{} = ability ->
        deactivate_ability(room, character, ability)

      nil ->
        message = "<p><span class='red'>Syntax: FORGET {ability}</span></p>"
        Mobile.send_scroll(character, message)

        room
    end
  end

  defp deactivate_ability(room, character, ability) do
    Room.update_mobile(room, character.ref, fn _room, character ->
      character_ability =
        Repo.get_by(CharacterAbility, character_id: character.id, ability_id: ability.id)

      message = "<p>You no longer know how to use #{ability.name}!</p>"

      Mobile.send_scroll(character, message)

      Repo.delete!(character_ability)

      Character.load_abilities(character)
    end)
  end
end
