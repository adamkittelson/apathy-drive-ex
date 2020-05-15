defmodule ApathyDrive.Trait do
  use ApathyDriveWeb, :model
  use GenServer

  alias ApathyDrive.Match

  require Logger

  schema "traits" do
    field(:name, :string)
    field(:description, :string)
    field(:merge_by, :string)

    has_many(:monsters_traits, ApathyDrive.MonsterTrait)
    has_many(:monsters, through: [:monsters_traits, :monster])

    has_many(:races_traits, ApathyDrive.MonsterTrait)
    has_many(:races, through: [:races_traits, :race])

    timestamps()
  end

  def start_link do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def init(state) do
    :ets.new(:traits, [
      :named_table,
      :set,
      read_concurrency: true,
      write_concurrency: true
    ])

    {:ok, state, {:continue, :populate_traits}}
  end

  def handle_continue(:populate_traits, state) do
    __MODULE__
    |> Ecto.Query.select([:name, :merge_by])
    |> Repo.all()
    |> Enum.each(fn trait ->
      :ets.insert(:traits, {trait.name, trait.merge_by})
    end)

    {:noreply, state}
  end

  def set_description_changeset(model, description) do
    model
    |> cast(%{description: description}, [:description])
    |> validate_required(:description)
    |> validate_length(:description, min: 10, max: 500)
  end

  def set_name_changeset(model, name) do
    model
    |> cast(%{name: name}, [:name])
    |> validate_required(:name)
    |> validate_length(:name, min: 1, max: 25)
  end

  def set_merge_by_changeset(model, merge_by) do
    model
    |> cast(%{merge_by: merge_by}, [:merge_by])
    |> validate_required(:merge_by)
    |> validate_inclusion(:merge_by, ["add", "multiply", "list", "replace", "mult%"])
  end

  def names do
    __MODULE__
    |> Ecto.Query.select([:name])
    |> Repo.all()
    |> Enum.map(& &1.name)
    |> Enum.sort()
  end

  def select do
    Repo.all(__MODULE__, select: [:id, :name])
    |> Enum.sort_by(& &1.name)
    |> Enum.map(&{&1.name, &1.id})
  end

  def merge_traits(traits1, traits2) do
    Enum.reduce(traits2, traits1, fn
      {trait, value}, traits ->
        if trait in Map.keys(traits) and !is_nil(value) do
          case merge_by(trait) do
            "add" ->
              Map.put(traits, trait, value + traits[trait])

            "multiply" ->
              Map.put(traits, trait, value * traits[trait])

            "list" ->
              Map.put(traits, trait, [value | List.wrap(traits[trait])])

            "replace" ->
              Map.put(traits, trait, value)

            "mult%" ->
              value = (1 - value / 100) * (1 - traits[trait] / 100)
              value = (1 - value) * 100
              Map.put(traits, trait, value)
          end
        else
          Map.put(traits, trait, value)
        end
    end)
  end

  def value(traits, name) do
    if value = traits[name] do
      cond do
        merge_by(name) == "list" ->
          List.flatten(value)

        :else ->
          value
      end
    else
      case merge_by(name) do
        "add" ->
          0

        "multiply" ->
          0

        "list" ->
          []

        "replace" ->
          nil

        "mult%" ->
          0
      end
    end
  end

  def merge_by(trait_name) do
    case :ets.lookup(:traits, trait_name) do
      [{^trait_name, merge_by}] ->
        merge_by

      _ ->
        cond do
          String.starts_with?(trait_name, "Resist") ->
            "add"

          trait_name in [
            "Retaliation",
            "stack_count",
            "stack_key",
            "timers",
            "Aggro",
            "effect_ref",
            "ClassLevel"
          ] ->
            "replace"

          :else ->
            Logger.info("#{trait_name} trait not found")
            "replace"
        end
    end
  end

  def match_by_name(name) do
    traits =
      __MODULE__
      |> ApathyDrive.Repo.all()

    Match.all(traits, :keyword_starts_with, name)
  end
end
