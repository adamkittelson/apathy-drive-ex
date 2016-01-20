defmodule ApathyDrive.ClassAbility do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Ability, Class}

  schema "class_abilities" do
    belongs_to :class, Class
    belongs_to :ability, Ability
    field :level, :integer

    timestamps
  end

  @required_fields ~w(class_id ability_id level)
  @optional_fields ~w()

  def changeset(model, params \\ :empty) do
    updated_params = update_params(params)

    model
    |> cast(updated_params, @required_fields, @optional_fields)
    |> unique_constraint(:class_id, name: :class_abilities_class_id_ability_id_index)
    |> foreign_key_constraint(:class_id)
    |> foreign_key_constraint(:ability_id)
  end

  def update_params(:empty), do: :empty
  def update_params(params) do
    params
    |> Map.put("class_id", get_number(Map.get(params, "class_id")))
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

  def for_spirit(%Spirit{level: level, class_id: class}) do
    __MODULE__
    |> where(class_id: ^class)
    |> where([ca], ca.level <= ^level)
    |> preload(:ability)
    |> ApathyDrive.Repo.all
    |> Enum.map(&(Map.merge(%{"level" => &1.level}, &1.ability.properties)))
  end

  def monsters_template_ids(room_id) do
    query = from lair in __MODULE__,
            where: lair.room_id == ^room_id,
            select: lair.monster_template_id

    query
    |> Repo.all
  end

  def ability_classes(ability_id) do
    __MODULE__
    |> where(ability_id: ^ability_id)
  end

  def names(class_abilities) when is_list(class_abilities) do
    class_abilities
    |> Enum.map(&Map.from_struct/1)
    |> Enum.map(&names/1)
  end

  def names(%{ability_id: ability_id, class_id: class_id} = class_ability) do
    class_ability
    |> Map.put(:ability, Repo.get(ApathyDrive.Ability, ability_id).properties["name"])
    |> Map.put(:class, Repo.get(ApathyDrive.Class, class_id).name)
  end

end
