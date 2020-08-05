defmodule ApathyDrive.ClassAbility do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Ability, AbilityAttribute, CharacterAbility, Class}

  schema "classes_abilities" do
    field(:level, :integer)
    field(:auto_learn, :boolean)

    belongs_to(:ability, Ability)
    belongs_to(:class, Class)
  end

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, ~w(damage_type_id kind potency)a)
  end

  def abilities_at_level(character, class_id, level) do
    leveled_abilities =
      ApathyDrive.ClassAbility
      |> Ecto.Query.where(
        [ss],
        ss.class_id == ^class_id and ss.level <= ^level
      )
      |> Ecto.Query.preload([:ability])
      |> Repo.all()
      |> Enum.filter(fn class_ability ->
        if class_ability.auto_learn do
          true
        else
          !!Repo.get_by(CharacterAbility,
            character_id: character.id,
            ability_id: class_ability.ability_id
          )
        end
      end)
      |> Enum.map(fn class_ability ->
        class_ability = put_in(class_ability.ability.level, class_ability.level)
        class_ability = put_in(class_ability.ability.class_id, class_id)

        put_in(
          class_ability.ability.attributes,
          AbilityAttribute.load_attributes(class_ability.ability.id)
        )
      end)

    base_abilities =
      ApathyDrive.ClassAbility
      |> Ecto.Query.where(
        [ss],
        ss.class_id == ^class_id and is_nil(ss.level) and ss.auto_learn == true
      )
      |> Ecto.Query.preload([:ability])
      |> Repo.all()
      |> Enum.map(fn class_ability ->
        class_ability = put_in(class_ability.ability.level, level)
        put_in(class_ability.ability.class_id, class_id)
      end)

    base_abilities ++ leveled_abilities
  end

  def load_damage(ability_id) do
    __MODULE__
    |> where([mt], mt.ability_id == ^ability_id)
    |> preload([:damage_type])
    |> Repo.all()
    |> Enum.reduce([], fn %{damage_type: damage_type, kind: kind, potency: potency}, damages ->
      [
        %{
          kind: kind,
          potency: potency,
          damage_type: damage_type.name,
          damage_type_id: damage_type.id
        }
        | damages
      ]
    end)
  end
end
