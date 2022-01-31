defmodule ApathyDrive.Commands.Skills do
  use ApathyDrive.Command
  alias ApathyDrive.{Character, Mobile, Skill}

  def keywords, do: ["sk", "skills"]

  def execute(%Room{} = room, %Character{} = character, args) do
    arg = Enum.join(args, "")

    Mobile.send_scroll(
      character,
      "<p><span class='white'>Your skills are:</span></p>"
    )

    Mobile.send_scroll(
      character,
      "<p><span class='dark-magenta'>---------------------------------------------------------------------------</span></p>"
    )

    character.skills
    |> Map.values()
    |> List.flatten()
    |> Enum.filter(&(&1.skill.type == "skill"))
    |> Enum.sort_by(& &1.skill.name)
    |> Enum.chunk_every(2)
    |> Enum.each(fn
      [skill1, skill2] ->
        name1 = name(skill1)

        amount1 = amount(skill1, character, arg)

        name2 = name(skill2)

        amount2 = amount(skill2, character, arg)

        Mobile.send_scroll(
          character,
          "<p>#{name1}#{amount1}       #{name2}#{amount2}</p>"
        )

      [skill] ->
        name = name(skill)

        amount = amount(skill, character, arg)

        Mobile.send_scroll(
          character,
          "<p>#{name}#{amount}</p>"
        )
    end)

    room
  end

  defp name(skill) do
    skill.skill.name
    |> to_string
    |> String.pad_trailing(30)
  end

  defp amount(skill, character, "base") do
    amount(Skill.module(skill.skill.name).base_skill_level(character))
  end

  defp amount(skill, character, _arg) do
    Mobile.ability_value(character, skill.skill.name)
  end

  defp amount(string) do
    String.pad_leading("#{string}%", 4)
  end
end
