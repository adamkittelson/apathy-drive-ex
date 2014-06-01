defmodule Systems.Trainer do
  def list(character, room) do
    header = "<span class='blue'>-=-=-=-=-=-=-=-</span>  <span class='white'>Skill Listing</span>  <span class='blue'>-=-=-=-=-=-=-=-</span>"
    Components.Player.send_message(character, ["scroll", "<p>#{header}</p>"])
    skills_by_level(room) |> Map.keys |> Enum.each fn level ->
      row = "Level#{String.rjust("#{level}", 3)} -------------------- Cost ----- Rating"
      Components.Player.send_message(character, ["scroll", "<p><span class='blue'>#{row}</span></p>"])
      skills_by_level(room)[level] |> Enum.each fn skill ->
        skill_name = Components.Name.value(skill) |> String.ljust(26)
        cost = "<span class='green'>#{"1" |> String.ljust(8)}</span>"
        rating = "#{"0" |> String.rjust(4)}</span>"
        row = "    #{skill_name}#{cost}#{rating}%"
        Components.Player.send_message(character, ["scroll", "<p>#{row}</p>"])
      end
    end
    footer = "<span class='blue'>-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-</span>"
    Components.Player.send_message(character, ["scroll", "<p>#{footer}</p>"])
  end

  defp skills_by_level(room) do
    room
    |> Components.Trainer.value
    |> Enum.reduce %{}, fn skill_name, skills ->
         skill = Skills.all[skill_name]
         level = Components.Level.value(skill)
         skills = Map.put_new(skills, level, [])
         Map.put(skills, level, [skill | skills[level]])
       end
  end
end