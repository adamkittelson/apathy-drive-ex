defmodule Commands.Skills do
  use ApathyDrive.Command

  def keywords, do: ["skills"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
  end

  def execute(%Monster{skills: skills} = monster, arguments) do
    arguments = Enum.join(arguments, " ")
    monster
    |> Monster.send_scroll("<p><span class='white'>Your skills are:</span></p>")
    |> Monster.send_scroll("<p><span class='blue'>------------------------------------------------------------------------</span></p>")

    skill_names = skills
                  |> Map.keys
                  |> Enum.sort

    chunks = get_chunks(skill_names)
    Enum.each chunks, &display_skills(monster, &1, arguments)
    monster
  end

  defp display_skills(%Monster{} = monster, [skill1, skill2], arguments) do
    monster
    |> Monster.send_scroll("<p>#{skilltext(monster, skill1, arguments)} #{skilltext(monster, skill2, arguments)}</p>")
  end

  defp display_skills(%Monster{} = monster, [skill], arguments) do
    monster
    |> Monster.send_scroll("<p>#{skilltext(monster, skill, arguments)}</p>")
  end

  defp skilltext(%Monster{} = monster, skill_name, "base") do
    base_skill_rating     = Monster.base_skill(monster, skill_name)
    modified_skill_rating = Monster.modified_skill(monster, skill_name)
    skill_difference = modified_skill_rating - base_skill_rating
    skill_mod = if skill_difference >= 0 do
                   String.ljust("(+#{skill_difference})", 6)
                 else
                   String.ljust("(#{skill_difference})", 6)
                 end

    String.ljust("#{String.ljust(skill_name, 24)}#{String.rjust("#{base_skill_rating}", 4)}% #{skill_mod}", 36)
  end

  defp skilltext(%Monster{} = monster, skill_name, _arguments) do
    skill_rating = Monster.modified_skill(monster, skill_name)
    String.ljust("#{String.ljust(skill_name, 24)}#{String.rjust("#{skill_rating}", 4)}%", 36)
  end

  defp get_chunks([]), do: []
  defp get_chunks(skills) do
    chunks = Enum.chunk(skills, 2)
    last_skill = skills |> List.last
    if List.flatten(chunks) |> Enum.member?(last_skill) do
      chunks
    else
      [[last_skill] | chunks |> Enum.reverse] |> Enum.reverse
    end
  end
end
