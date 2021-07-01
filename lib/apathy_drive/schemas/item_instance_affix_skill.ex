defmodule ApathyDrive.ItemInstanceAffixSkill do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{AffixSkill, ItemInstance, ItemInstanceAffixSkill, Trait}

  schema "items_instances_affix_skills" do
    belongs_to(:item_instance, ItemInstance)
    belongs_to(:affix_skill, AffixSkill)

    field(:value, ApathyDrive.JSONB)
    field(:description, :string)
  end

  def load_skills(nil, _item), do: %{}

  def load_skills(item_instance_id, _item) do
    __MODULE__
    |> where([mt], mt.item_instance_id == ^item_instance_id)
    |> preload(affix_skill: [:skill])
    |> Repo.all()
    |> Enum.reduce(%{}, fn
      %ItemInstanceAffixSkill{
        value: %{"kind" => kind} = value,
        affix_skill: %{skill: %{} = skill}
      },
      skills
      when kind in ["OnHit", "OnAttack", "WhenStruck"] ->
        trait = %{
          kind => [
            %{"chance" => value["chance"], "level" => value["level"], "skill_id" => skill.id}
          ]
        }

        Trait.merge_traits(skills, trait)

      %ItemInstanceAffixSkill{value: %{} = value, affix_skill: %{skill: %{} = _skill}}, skills ->
        skills
    end)
  end
end
