defmodule ApathyDrive.Commands.System.Edit do
  alias ApathyDrive.{Ability, Mobile, Room, Skill}

  def execute(%Room{} = room, character, ["ability" | ability_name]) do
    ability_name = Enum.join(ability_name, " ")

    case Ability.match_by_name(ability_name, true) do
      nil ->
        Mobile.send_scroll(character, "<p>\"#{ability_name}\" does not match any abilities.</p>")
        room

      %Ability{} = ability ->
        Mobile.send_scroll(character, "<p>You are now editing #{ability.name}.</p>")

        Room.update_mobile(room, character.ref, fn _room, character ->
          character
          |> Map.put(:editing, ability)
          |> Mobile.update_prompt(room)
        end)

      matches ->
        Mobile.send_scroll(
          character,
          "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
        )

        Enum.each(matches, fn match ->
          Mobile.send_scroll(character, "<p>-- #{match.name}</p>")
        end)

        room
    end
  end

  def execute(%Room{} = room, character, ["skill" | skill_name]) do
    skill_name = Enum.join(skill_name, " ")

    case Skill.match_by_name(skill_name, true) do
      nil ->
        Mobile.send_scroll(character, "<p>\"#{skill_name}\" does not match any skills.</p>")
        room

      %Skill{} = skill ->
        Mobile.send_scroll(character, "<p>You are now editing #{skill.name}.</p>")

        Room.update_mobile(room, character.ref, fn _room, character ->
          character
          |> Map.put(:editing, skill)
          |> Mobile.update_prompt(room)
        end)

      matches ->
        Mobile.send_scroll(
          character,
          "<p><span class='red'>Please be more specific. You could have meant any of these:</span></p>"
        )

        Enum.each(matches, fn match ->
          Mobile.send_scroll(character, "<p>-- #{match.name}</p>")
        end)

        room
    end
  end

  def execute(%Room{} = room, character, _args) do
    Mobile.send_scroll(character, "<p>Invalid system command.</p>")

    room
  end
end
