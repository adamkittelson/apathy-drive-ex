defmodule ApathyDrive.MonsterAbility do
  use ApathyDrive.Web, :model
  alias ApathyDrive.Ability

  schema "monster_abilities" do
    belongs_to :monster_template, MonsterTemplate
    belongs_to :ability, Ability

    timestamps
  end

  @required_fields ~w(monster_template_id ability_id)
  @optional_fields ~w()

  def changeset(model, params \\ :empty) do
    updated_params = update_params(params)

    model
    |> cast(updated_params, @required_fields, @optional_fields)
    |> unique_constraint(:monster_template_id, name: :monster_abilities_monster_template_id_ability_id_index)
    |> foreign_key_constraint(:monster_template_id)
    |> foreign_key_constraint(:ability_id)
  end

  def update_params(:empty), do: :empty
  def update_params(params) do
    params
    |> Map.put("monster_template_id", get_number(Map.get(params, "monster_template_id")))
    |> Map.put("ability_id", get_number(Map.get(params, "ability_id")))
  end

  def get_number(nil), do: nil
  def get_number(""),  do: nil
  def get_number(string) do
    case Regex.run(~r/\d+$/, string) do
      nil ->
        nil
      [number] ->
        number
    end
  end

  def ability_monsters(ability_id) do
    __MODULE__
    |> where(ability_id: ^ability_id)
  end

  def names(monster_abilities) when is_list(monster_abilities) do
    monster_abilities
    |> Enum.map(&Map.from_struct/1)
    |> Enum.map(&names/1)
  end

  def names(%{ability_id: ability_id, monster_template_id: monster_template_id} = monster_ability) do
    monster_ability
    |> Map.put(:ability, Repo.get(ApathyDrive.Ability, ability_id).properties["name"])
    |> Map.put(:monster, Repo.get(MonsterTemplate, monster_template_id).name)
  end

end
