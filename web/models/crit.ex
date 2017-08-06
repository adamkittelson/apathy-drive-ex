defmodule ApathyDrive.Crit do
  use ApathyDrive.Web, :model
  use Timex

  schema "crits" do
    field :crit_table, :string
    field :letter, :string
    field :abilities, ApathyDrive.JSONB, default: []

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

  def abilities(damage, crit_tables) do
    table = Enum.random(crit_tables)
    letter = roll_for_letter(damage)

    if letter do
      count =
        __MODULE__
        |> where(crit_table: ^table, letter: ^letter)
        |> select([crit], count(crit.id))
        |> Repo.one

      __MODULE__
      |> where(crit_table: ^table, letter: ^letter)
      |> offset(fragment("floor(random()*?) LIMIT 1", ^count))
      |> select([crit], crit.abilities)
      |> Repo.one
    else
      []
    end
  end

  def roll_for_letter(crit_chance) do
    case :rand.uniform(1_000_000) do
      roll when roll > crit_chance * 10_000 ->
        nil
      roll when roll > crit_chance * 5000 ->
        "A"
      roll when roll > crit_chance * 2500 ->
        "B"
      roll when roll > crit_chance * 1250 ->
        "C"
      roll when roll > crit_chance * 625 ->
        "D"
      _ ->
        "E"
    end
  end

end
