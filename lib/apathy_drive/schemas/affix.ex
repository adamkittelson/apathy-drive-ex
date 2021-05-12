defmodule ApathyDrive.Affix do
  use ApathyDriveWeb, :model
  require Ecto.Query

  alias ApathyDrive.{AffixItemType, ItemInstanceAffixTrait}

  schema "affixes" do
    field(:type, :string)
    field(:name, :string)
    field(:spawnable, :boolean)
    field(:rare, :boolean)
    field(:level, :integer)
    field(:max_level, :integer)
    field(:required_level, :integer)
    field(:frequency, :integer)
    field(:group, :integer)

    has_many(:affixes_traits, ApathyDrive.AffixTrait)
    has_many(:affix_skills, ApathyDrive.AffixSkill)

    timestamps()
  end

  def prefix_for_level(affix_level, item_instance) do
    affix_groups_on_item = ItemInstanceAffixTrait.affix_groups_on_item(item_instance, "prefix")

    IO.inspect(item_instance.item.name)

    group =
      prefix_group_for_level(affix_level, affix_groups_on_item, item_instance.item.item_types)

    if group do
      __MODULE__
      |> Ecto.Query.where([a], a.type == "prefix")
      |> affix_for_group_at_level(group, affix_level, item_instance.item.item_types)
    end
  end

  def suffix_for_level(affix_level, item_instance) do
    affix_groups_on_item = ItemInstanceAffixTrait.affix_groups_on_item(item_instance, "suffix")

    IO.inspect(item_instance.item.name)

    group =
      suffix_group_for_level(affix_level, affix_groups_on_item, item_instance.item.item_types)

    if group do
      __MODULE__
      |> Ecto.Query.where([a], a.type == "suffix")
      |> affix_for_group_at_level(group, affix_level, item_instance.item.item_types)
    end
  end

  def prefix_for_group_at_level(group, affix_level, item_types) do
    __MODULE__
    |> Ecto.Query.where([a], a.type == "prefix")
    |> affix_for_group_at_level(group, affix_level, item_types)
  end

  def suffix_for_group_at_level(group, affix_level, item_types) do
    __MODULE__
    |> Ecto.Query.where([a], a.type == "prefix")
    |> affix_for_group_at_level(group, affix_level, item_types)
  end

  def prefix_group_for_level(affix_level, prefix_groups_on_item, item_types) do
    __MODULE__
    |> Ecto.Query.where([a], a.type == "prefix")
    |> Ecto.Query.where([a], a.group not in ^prefix_groups_on_item)
    |> affix_group_for_level(affix_level, item_types)
  end

  def suffix_group_for_level(affix_level, suffix_groups_on_item, item_types) do
    __MODULE__
    |> Ecto.Query.where([a], a.type == "suffix")
    |> Ecto.Query.where([a], a.group not in ^suffix_groups_on_item)
    |> affix_group_for_level(affix_level, item_types)
  end

  defp affix_for_group_at_level(query, group, affix_level, item_types) do
    affixes =
      query
      |> Ecto.Query.where(
        [a],
        a.level <= ^affix_level and (a.max_level >= ^affix_level or is_nil(a.max_level)) and
          a.group == ^group and
          a.frequency >= 1 and not is_nil(a.frequency)
      )
      |> Repo.all()
      |> Enum.reject(&affix_not_allowed?(&1, item_types))

    total =
      affixes
      |> Enum.map(& &1.frequency)
      |> Enum.sum()

    roll = :rand.uniform(total)

    affixes
    |> Enum.reduce_while(0, fn affix, num ->
      num = num + affix.frequency

      if roll <= num do
        {:halt, affix}
      else
        {:cont, num}
      end
    end)
  end

  defp affix_group_for_level(query, affix_level, item_types) do
    query
    |> Ecto.Query.where(
      [a],
      a.level <= ^affix_level and (a.max_level >= ^affix_level or is_nil(a.max_level)) and
        a.frequency >= 1 and not is_nil(a.frequency)
    )
    |> Repo.all()
    |> Enum.reject(&affix_not_allowed?(&1, item_types))
    |> Enum.map(& &1.group)
    |> case do
      [] ->
        nil

      affixes ->
        affixes
        |> Enum.random()
    end
  end

  def affix_not_allowed?(%__MODULE__{id: id} = _affix, item_types) do
    allowed =
      Enum.map(item_types, fn item_type ->
        AffixItemType
        |> Ecto.Query.where(affix_id: ^id, item_type_id: ^item_type.id, allowed: true)
        |> Ecto.Query.preload([:affix, :item_type])
        |> Repo.all()
      end)
      |> List.flatten()

    Enum.each(
      allowed,
      &IO.puts("#{&1.affix.name}(#{&1.affix_id}) allowed for #{&1.item_type.name}")
    )

    if Enum.any?(allowed) do
      disallowed =
        Enum.map(item_types, fn item_type ->
          AffixItemType
          |> Ecto.Query.where(affix_id: ^id, item_type_id: ^item_type.id, allowed: false)
          |> Ecto.Query.preload([:affix, :item_type])
          |> Repo.all()
        end)
        |> List.flatten()

      Enum.each(
        disallowed,
        &IO.puts("#{&1.affix.name}(#{&1.affix_id}) not allowed for #{&1.item_type.name}")
      )

      disallowed
      |> Enum.any?()
    else
      true
    end
  end
end
