defmodule Commands.Skills do
  use Systems.Command

  def keywords, do: ["skills"]

  def execute(entity, _arguments) do
    Components.Player.send_message(entity, ["scroll", "<p><span class='white'>Your skills are:</span></p>"])
    Components.Player.send_message(entity, ["scroll", "<p><span class='blue'>---------------------------------------------------------------------------</span></p>"])
    skill_names = Components.Skills.value(entity) |> Map.keys |> Enum.sort
    chunks = get_chunks(skill_names)
    Enum.each chunks, &display_skills(entity, &1)
  end

  defp display_skills(entity, [skill1, skill2]) do
    skill1_rating = Skills.all[skill1]
                    |> Systems.Trainer.rating(entity)

    skill2_rating = Skills.all[skill2]
                    |> Systems.Trainer.rating(entity)
    message = "#{String.ljust(skill1, 24)}#{String.rjust("#{skill1_rating}", 4)}%        #{String.ljust(skill2, 24)}#{String.rjust("#{skill2_rating}", 4)}%"
    Components.Player.send_message(entity, ["scroll", "<p>#{message}</p>"])
  end

  defp display_skills(entity, [skill]) do
    skill_rating = Skills.all[skill]
                   |> Systems.Trainer.rating(entity)
    message = "#{String.ljust(skill, 24)}#{String.rjust("#{skill_rating}", 4)}%"
    Components.Player.send_message(entity, ["scroll", "<p>#{message}</p>"])
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
