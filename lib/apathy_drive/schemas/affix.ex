defmodule ApathyDrive.Affix do
  use ApathyDriveWeb, :model
  require Ecto.Query

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

    timestamps()
  end

  def prefix_for_level(affix_level) do
    group = prefix_group_for_level(affix_level)

    __MODULE__
    |> Ecto.Query.where([a], a.type == "prefix")
    |> affix_for_group_at_level(group, affix_level)
  end

  def suffix_for_level(affix_level) do
    group = suffix_group_for_level(affix_level)

    __MODULE__
    |> Ecto.Query.where([a], a.type == "suffix")
    |> affix_for_group_at_level(group, affix_level)
  end

  def prefix_for_group_at_level(group, affix_level) do
    __MODULE__
    |> Ecto.Query.where([a], a.type == "prefix")
    |> affix_for_group_at_level(group, affix_level)
  end

  def suffix_for_group_at_level(group, affix_level) do
    __MODULE__
    |> Ecto.Query.where([a], a.type == "prefix")
    |> affix_for_group_at_level(group, affix_level)
  end

  def prefix_group_for_level(affix_level) do
    __MODULE__
    |> Ecto.Query.where([a], a.type == "prefix")
    |> affix_group_for_level(affix_level)
  end

  def suffix_group_for_level(affix_level) do
    __MODULE__
    |> Ecto.Query.where([a], a.type == "suffix")
    |> affix_group_for_level(affix_level)
  end

  defp affix_for_group_at_level(query, group, affix_level) do
    affixes =
      query
      |> Ecto.Query.where(
        [a],
        a.level <= ^affix_level and a.max_level >= ^affix_level and a.group == ^group and
          a.frequency >= 1
      )
      |> Repo.all()

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

  defp affix_group_for_level(query, affix_level) do
    query
    |> Ecto.Query.where(
      [a],
      a.level <= ^affix_level and a.max_level >= ^affix_level and a.frequency >= 1
    )
    |> Ecto.Query.select([:group])
    |> Repo.all()
    |> Enum.map(& &1.group)
    |> Enum.random()
  end
end
