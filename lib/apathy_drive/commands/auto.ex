defmodule ApathyDrive.Commands.Auto do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, CharacterSkill, Mobile, Repo}

  def keywords, do: ["auto"]

  def execute(%Room{} = room, %Character{} = character, []) do
    Mobile.send_scroll(
      character,
      "<p><span class='white'>You have the following automation settings:</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-magenta'>Setting   Enabled?</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-cyan'>Attack       #{emoji(character.auto_attack)}</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-cyan'>Roam         #{emoji(character.auto_roam)}</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-cyan'>Sneak        #{emoji(character.auto_sneak)}</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-cyan'>Rest         #{emoji(character.auto_rest)}</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-cyan'>Pet Casting  #{emoji(character.auto_pet_casting)}</span></p>"
    )

    room
  end

  def execute(%Room{} = room, %Character{ref: ref} = character, ["attack"]) do
    Mobile.send_scroll(character, "<p>Toggling auto attack.</p>")

    room =
      Room.update_mobile(room, ref, fn _room, character ->
        character
        |> Ecto.Changeset.change(%{auto_attack: !character.auto_attack})
        |> Repo.update!()
      end)

    execute(room, room.mobiles[ref], [])
  end

  def execute(%Room{} = room, %Character{ref: ref} = character, ["roam"]) do
    Mobile.send_scroll(character, "<p>Toggling auto roam.</p>")

    room =
      Room.update_mobile(room, ref, fn _room, character ->
        character
        |> Ecto.Changeset.change(%{auto_roam: !character.auto_roam})
        |> Repo.update!()
      end)

    execute(room, room.mobiles[ref], [])
  end

  def execute(%Room{} = room, %Character{ref: ref} = character, ["sneak"]) do
    Mobile.send_scroll(character, "<p>Toggling auto sneak.</p>")

    room =
      Room.update_mobile(room, ref, fn _room, character ->
        character
        |> Ecto.Changeset.change(%{auto_sneak: !character.auto_sneak})
        |> Repo.update!()
      end)

    execute(room, room.mobiles[ref], [])
  end

  def execute(%Room{} = room, %Character{ref: ref} = character, ["rest"]) do
    Mobile.send_scroll(character, "<p>Toggling auto rest.</p>")

    room =
      Room.update_mobile(room, ref, fn _room, character ->
        character
        |> Ecto.Changeset.change(%{auto_rest: !character.auto_rest})
        |> Repo.update!()
      end)

    execute(room, room.mobiles[ref], [])
  end

  def execute(%Room{} = room, %Character{ref: ref} = character, ["pet", "casting"]) do
    Mobile.send_scroll(character, "<p>Toggling auto pet casting.</p>")

    room =
      Room.update_mobile(room, ref, fn _room, character ->
        character
        |> Ecto.Changeset.change(%{auto_pet_casting: !character.auto_pet_casting})
        |> Repo.update!()
      end)

    execute(room, room.mobiles[ref], [])
  end

  def execute(%Room{} = room, %Character{skills: skills} = character, [command]) do
    if skill = skills[String.downcase(command)] do
      IO.puts("character_id: #{character.id}, skill: #{inspect(skill)}")

      case Repo.get_by(CharacterSkill, character_id: character.id, skill_id: skill.skill_id) do
        nil ->
          %CharacterSkill{character_id: character.id, skill_id: skill.skill_id, auto: true}
          |> Repo.insert!()

        ca ->
          ca
          |> Ecto.Changeset.change(%{auto: !ca.auto})
          |> Repo.update!()
      end

      room = update_in(room.mobiles[character.ref], &Character.load_skills/1)

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

  def emoji(true), do: "✅"
  def emoji(_), do: "❌"
end
