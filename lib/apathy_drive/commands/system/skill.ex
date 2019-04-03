defmodule ApathyDrive.Commands.System.Skill do
  alias ApathyDrive.{Ability, Mobile, Repo, Room, Skill, SkillAbility, SkillIncompatibility}
  require Ecto.Query

  def execute(%Room{} = room, character, ["add", "ability" | args]) do
    level = List.last(args)

    ability =
      args
      |> List.delete(level)
      |> Enum.join(" ")

    add_ability_to_skill(room, character, ability, level)
    room
  end

  def execute(%Room{} = room, character, ["create" | skill_name]) do
    create(room, character, skill_name)
  end

  def execute(%Room{} = room, character, ["set", "incompatible" | skills]) do
    if length(skills) >= 3 and Enum.member?(skills, "and") do
      set_incompatible(room, character, skills)
    else
      Mobile.send_scroll(
        character,
        "<p>You must give two skills seperated by 'and', e.g. 'blade and blunt'.</p>"
      )

      room
    end
  end

  def execute(%Room{} = room, character, ["list"]) do
    list(room, character)
  end

  def execute(%Room{} = room, character, _args) do
    Mobile.send_scroll(character, "<p>Invalid system command.</p>")

    room
  end

  def create(%Room{} = room, character, skill_name) do
    skill = Enum.join(skill_name, " ")

    skill
    |> Skill.create_changeset()
    |> Repo.insert()
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
    skills =
      Skill
      |> Ecto.Query.order_by(asc: :id)
      |> Repo.all()

    padding =
      skills
      |> Enum.map(&String.length(&1.name))
      |> Enum.max()

    Mobile.send_scroll(
      character,
      "<p>#{String.pad_leading("", padding + 1)}<span class='dark-green'>|</span> <span class='dark-magenta'>Cost</span> <span class='dark-green'>|</span> <span class='dark-magenta'>Incompatible With</span></p>"
    )

    skills
    |> Enum.each(fn skill ->
      skill = Repo.preload(skill, :incompatible_skills)

      incompatible_skills =
        skill.incompatible_skills
        |> Enum.map(& &1.name)
        |> Enum.join(", ")

      Mobile.send_scroll(
        character,
        "<p><span class='dark-cyan'>#{String.pad_leading(skill.name, padding)}</span> <span class='dark-green'>|</span> <span class='dark-cyan'>#{
          String.pad_leading(to_string(skill.training_cost_multiplier), 4)
        }</span> <span class='dark-green'>|</span> <span class='black'>#{incompatible_skills}</span></p>"
      )
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
        Repo.insert_all(
          SkillIncompatibility,
          [
            [skill_id: skill.id, incompatible_skill_id: incompatible_skill.id],
            [skill_id: incompatible_skill.id, incompatible_skill_id: skill.id]
          ],
          on_conflict: :nothing
        )
        |> case do
          {2, nil} ->
            Mobile.send_scroll(
              character,
              "<p>#{skill.name} is now incompatible with #{incompatible_skill.name}.</p>"
            )

          {0, nil} ->
            Mobile.send_scroll(
              character,
              "<p>#{skill.name} is already incompatible with #{incompatible_skill.name}.</p>"
            )

          error ->
            Mobile.send_scroll(character, "<p>#{inspect(error)}</p>")
        end
    end

    room
  end

  def add_ability_to_skill(%Room{} = room, character, ability, level) do
    with %Ability{} = ability <- Ability.match_by_name(ability),
         {level, _decimal} <- Integer.parse(level) do
      skill = character.editing

      Repo.insert!(%SkillAbility{skill_id: skill.id, ability_id: ability.id, level: level})

      ApathyDrive.PubSub.broadcast!("rooms", :reload_abilities)

      Mobile.send_scroll(
        character,
        "<p>Added #{ability.name} to #{skill.name} at level #{level}.</p>"
      )
    else
      nil ->
        Mobile.send_scroll(character, "<p>#{ability} did not match a known ability.</p>")

      :error ->
        Mobile.send_scroll(character, "<p>Level must be an integer.</p>")
    end

    room
  end
end
