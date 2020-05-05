defmodule ApathyDrive.ItemAbility do
  use ApathyDriveWeb, :model

  alias ApathyDrive.{
    Ability,
    AbilityDamageType,
    AbilityTrait,
    ItemAbility,
    Item,
    Trait
  }

  schema "items_abilities" do
    field(:delete, :boolean, virtual: true)

    belongs_to(:item, Item)
    belongs_to(:ability, Ability)
    belongs_to(:type, ApathyDrive.ItemAbilityType)
  end

  @required_fields ~w(item_id ability_id)a

  def load_abilities(%Item{id: id} = item) do
    traits =
      __MODULE__
      |> where([ia], ia.item_id == ^id)
      |> preload([:type, :ability])
      |> Repo.all()
      |> Enum.reduce(%{}, fn item_ability, traits ->
        ability = item_ability.ability

        ability = put_in(ability.traits, AbilityTrait.load_traits(ability.id))

        ability =
          case AbilityDamageType.load_damage(ability.id) do
            [] ->
              ability

            damage ->
              ability = update_in(ability.traits, &Map.put(&1, "Damage", damage))

              crit_types = Enum.map(ability.traits["Damage"], & &1.damage_type_id)

              Map.put(ability, :crit_tables, crit_types)
          end

        ability_traits =
          if item_ability.type.name == "OnHit" do
            traits
            |> Map.put_new("OnHit", [])
            |> update_in(["OnHit"], &[ability | &1])
          else
            Map.put(traits, item_ability.type.name, ability)
          end

        Trait.merge_traits(traits, ability_traits)
      end)

    Systems.Effect.add(item, traits)
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
