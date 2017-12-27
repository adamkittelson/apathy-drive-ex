defmodule ApathyDrive.Crit do
  use ApathyDrive.Web, :model
  use Timex

  alias ApathyDrive.{Ability, CritTrait, DamageType}

  schema "crits" do
    field :crit_table, :string
    field :letter, :string
    field :abilities, ApathyDrive.JSONB, default: []
    field :user_message, :string
    field :target_message, :string
    field :spectator_message, :string
    field :duration, :integer, default: 0

    belongs_to :damage_type, DamageType

    timestamps()
  end

  def changeset(model, params \\ %{}) do
    model
    |> cast(params, ["crit_table", "letter", "abilities"], [])
    |> validate_inclusion(:letter, ["A", "B", "C", "D", "E"])
  end

  def for_table(table) do
    __MODULE__
    |> where(crit_table: ^table)
  end

  def find_for_ability(%Ability{} = ability, damage) do
    table = table_for_ability(ability)
    letter = roll_for_letter()

    if table && letter do
      count =
        __MODULE__
        |> where(damage_type_id: ^table, letter: ^letter)
        |> select([crit], count(crit.id))
        |> Repo.one

      crit =
        __MODULE__
        |> where(damage_type_id: ^table, letter: ^letter)
        |> offset(fragment("floor(random()*?) LIMIT 1", ^count))
        |> Repo.one

      if crit do
        ability =
          %Ability{
            duration: crit.duration,
            kind: "critical",
            user_message: crit.user_message,
            target_message: crit.target_message,
            spectator_message: crit.spectator_message
          }

        case CritTrait.load_traits(crit.id) do
          %{"DamageMultiplier" => multiplier} = traits ->
            traits =
              traits
              |> Map.delete("DamageMultiplier")
              |> Map.put("Damage", damage * multiplier)
            put_in(ability.traits, traits)
          %{} = traits ->
            traits = Map.put(traits, "Damage", damage)
            put_in(ability.traits, traits)
        end
      end
    end
  end

  def table_for_ability(%Ability{traits: %{"Damage" => damages}}) do
    {total, damages} =
      damages
      |> Enum.reduce({0, []}, fn %{potency: potency, damage_type_id: id}, {total, list} ->
        {total + potency, [{total + potency, id} | list]}
      end)

    roll =
      total
      |> trunc
      |> :rand.uniform

    case Enum.find(Enum.reverse(damages), fn {n, _id} -> roll <= n end) do
      {_, id} ->
        id
      _ ->
        nil
    end
  end

  def roll_for_letter do
    case :rand.uniform(1_000_000) do
      roll when roll <= 100 ->
        "E"
      roll when roll <= 1000 ->
        "D"
      roll when roll <= 10_000 ->
        "C"
      roll when roll <= 100_000 ->
        "B"
      _ ->
        "A"
    end
  end

end
