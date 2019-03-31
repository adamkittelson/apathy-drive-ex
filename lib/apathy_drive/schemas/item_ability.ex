defmodule ApathyDrive.ItemAbility do
  use ApathyDriveWeb, :model

  alias ApathyDrive.{
    Ability,
    AbilityAttribute,
    AbilityDamageType,
    AbilityTrait,
    ItemAbility,
    Item
  }

  schema "items_abilities" do
    field(:delete, :boolean, virtual: true)

    belongs_to(:item, Item)
    belongs_to(:ability, Ability)
    belongs_to(:type, ApathyDrive.ItemAbilityType)
  end

  @required_fields ~w(item_id ability_id)a

  def load_abilities(%Item{id: id, traits: traits} = item) do
    traits =
      __MODULE__
      |> where([ia], ia.item_id == ^id)
      |> preload([:type, :ability])
      |> Repo.all()
      |> Enum.reduce(traits, fn item_ability, traits ->
        attributes = AbilityAttribute.load_attributes(item_ability.ability.id)
        ability = Map.put(item_ability.ability, :attributes, attributes)

        ability = put_in(ability.traits, AbilityTrait.load_traits(ability.id))

        ability =
          case AbilityDamageType.load_damage(ability.id) do
            [] ->
              ability

            damage ->
              update_in(ability.traits, &Map.put(&1, "Damage", damage))
          end

        Map.put(traits, item_ability.type.name, ability)
      end)

    Map.put(item, :traits, traits)
  end

  def changeset(%ItemAbility{} = rt, attrs) do
    rt
    |> cast(attrs, [:delete | @required_fields])
    |> validate_required(@required_fields)
    |> mark_for_deletion()
  end

  defp mark_for_deletion(changeset) do
    # If delete was set and it is true, let's change the action
    if get_change(changeset, :delete) do
      %{changeset | action: :delete}
    else
      changeset
    end
  end
end
