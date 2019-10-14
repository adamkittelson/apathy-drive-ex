defmodule ApathyDrive.Commands.Auto do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, CharacterAbility, Mobile, Repo}

  def keywords, do: ["auto"]

  def execute(%Room{} = room, %Character{abilities: abilities} = character, [command]) do
    if ability = abilities[String.downcase(command)] do
      case Repo.get_by(CharacterAbility, character_id: character.id, ability_id: ability.id) do
        nil ->
          %CharacterAbility{character_id: character.id, ability_id: ability.id, auto: true}
          |> Repo.insert!()

        ca ->
          ca
          |> CharacterAbility.changeset(%{auto: !ca.auto})
          |> Repo.update!()
      end

      room =
        update_in(room.mobiles[character.ref].abilities[String.downcase(command)].auto, &(!&1))

      ApathyDrive.Commands.Abilities.execute(room, room.mobiles[character.ref], [])
    else
      message = "<p><span class='red'>You don't know #{command}.</span></p>"

      Mobile.send_scroll(character, message)

      room
    end
  end

  def execute(%Room{} = room, %Character{} = character, _) do
    message =
      "<p><span class='red'>Syntax: 'AUTO {ability command}', e.g. 'auto mmis' to toggle automatically casting magic missile.</span></p>"

    Mobile.send_scroll(character, message)

    room
  end
end
