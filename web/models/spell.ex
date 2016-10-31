defmodule ApathyDrive.Spell do
  use ApathyDrive.Web, :model
  alias ApathyDrive.Spell

  schema "spells" do
    field :name, :string
    field :targets, :string
    field :kind, :string
    field :mana, :integer
    field :command, :string
    field :description, :string
    field :user_message, :string
    field :target_message, :string
    field :spectator_message, :string
    field :duration_in_ms, :integer

    has_many :characters, ApathyDrive.Character

    has_many :spells_abilities, ApathyDrive.SpellAbility
    has_many :abilities, through: [:spells_abilities, :ability]

    timestamps
  end

  @required_fields ~w(name targets kind mana command description user_message target_message spectator_message duration_in_ms)
  @optional_fields ~w()

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields, @optional_fields)
  end
  
  def base_mana_at_level(level), do: 500 + ((level - 1) * 50)

  def mana_cost_at_level(%Spell{mana: mana} = spell, level) do
    trunc(base_mana_at_level(level) * (mana / 100))
  end

end
