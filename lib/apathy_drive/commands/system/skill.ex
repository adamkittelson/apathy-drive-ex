defmodule ApathyDrive.Commands.System.Skill do
  alias ApathyDrive.{Mobile, Repo, Room, Skill, SkillIncompatibility}
  require Ecto.Query


  def create(%Room{} = room, character, skill_name) do
    skill = Enum.join(skill_name, " ")

    skill
    |> Skill.create_changeset
    |> Repo.insert
    |> case do
         {:ok, %Skill{name: name}} ->
           Mobile.send_scroll(character, "<p>\"#{name}\" created!</p>")
           room
         {:error, %Ecto.Changeset{errors: errors}} ->
           Enum.each(errors, fn {field, error} ->
             message = ApathyDriveWeb.ErrorHelpers.translate_error(error)
             Mobile.send_scroll(character, "<p>Error: #{field} #{message}</p>")
           end)
           room
       end
  end

  def list(%Room{} = room, character) do
    skills = Repo.all(Skill)

    padding =
      skills
      |> Enum.map(& String.length(&1.name))
      |> Enum.max

    Mobile.send_scroll(character, "<p>#{String.pad_leading("", padding + 1)}<span class='dark-green'>|</span> <span class='dark-magenta'>Cost</span> <span class='dark-green'>|</span> <span class='dark-magenta'>Incompatible With</span></p>")

    skills
    |> Enum.each(fn skill ->
         skill = Repo.preload(skill, :incompatible_skills)
         incompatible_skills =
           skill.incompatible_skills
           |> Enum.map(& &1.name)
           |> Enum.join(", ")
         Mobile.send_scroll(character, "<p><span class='dark-cyan'>#{String.pad_leading(skill.name, padding)}</span> <span class='dark-green'>|</span> <span class='dark-cyan'>#{String.pad_leading(to_string(skill.training_cost_multiplier), 4)}</span> <span class='dark-green'>|</span> <span class='black'>#{incompatible_skills}</span></p>")
       end)

    room
  end

  def set_incompatible(%Room{} = room, character, skills) do
    [skill, incompatible_skill] =
      skills
      |> Enum.join(" ")
      |> String.split(" and ")
      |> Enum.map(&Skill.match_by_name/1)

    cond do
      is_nil(skill) or is_nil(incompatible_skill) ->
        Mobile.send_scroll(character, "<p>Could not find one of those skills.</p>")
      skill.id == incompatible_skill.id ->
        Mobile.send_scroll(character, "<p>A skill cannot be incompatible with itself.</p>")
      :else ->

        Repo.insert_all(SkillIncompatibility, [[skill_id: skill.id, incompatible_skill_id: incompatible_skill.id], [skill_id: incompatible_skill.id, incompatible_skill_id: skill.id]], on_conflict: :nothing)
        |> case do
          {2, nil} ->
            Mobile.send_scroll(character, "<p>#{skill.name} is now incompatible with #{incompatible_skill.name}.</p>")
          {0, nil} ->
            Mobile.send_scroll(character, "<p>#{skill.name} is already incompatible with #{incompatible_skill.name}.</p>")
          error ->
            Mobile.send_scroll(character, "<p>#{inspect(error)}</p>")
        end
    end
    room
  end

end
