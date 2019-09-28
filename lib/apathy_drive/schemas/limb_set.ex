defmodule ApathyDrive.LimbSet do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{LimbSet, LimbSetLimb}

  schema "limb_sets" do
    field(:name, :string)

    has_many(:limb_set_limbs, LimbSetLimb)
  end

  @required_fields ~w(name)a

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(%LimbSet{} = race, attrs \\ %{}) do
    race
    |> cast(attrs, @required_fields)
    |> validate_required(@required_fields)
    |> cast_assoc(:limb_sets)
  end

  def load_limbs(mobile, limb_set_id) do
    LimbSetLimb
    |> where(limb_set_id: ^limb_set_id)
    |> preload(:limb)
    |> preload(parent_limb: [:limb])
    |> preload(limb_set_limb_slots: [:slot])
    |> Repo.all()
    |> Enum.reduce(%{}, fn limb, limbs ->
      limb_name = limb_name(limb)
      health = if limb_name in mobile.missing_limbs, do: 0.0, else: 1.0

      limb =
        cond do
          limb.depends_on ->
            %{
              health: health,
              parent: limb_name(limb.parent_limb),
              fatal: limb.fatal,
              slots: Enum.map(limb.limb_set_limb_slots, & &1.slot.name),
              type: limb.limb.type,
              name: limb.limb.name
            }

          :else ->
            %{
              health: health,
              fatal: limb.fatal,
              slots: Enum.map(limb.limb_set_limb_slots, & &1.slot.name),
              type: limb.limb.type,
              name: limb.limb.name
            }
        end

      Map.put(limbs, limb_name, limb)
    end)
  end

  defp limb_name(limb) do
    [limb.location, limb.limb.name]
    |> Enum.reject(&is_nil/1)
    |> Enum.join(" ")
  end
end
