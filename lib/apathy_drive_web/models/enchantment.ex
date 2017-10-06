defmodule ApathyDrive.Enchantment do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{ItemInstance, Ability, AbilityDamageType, AbilityTrait}

  schema "enchantments" do
    field :enchanted_by, :string
    field :progress, :float
    belongs_to :items_instances, ItemInstance
    belongs_to :ability, Ability
  end

  def load_damage(nil), do: []
  def load_damage(instance_id) do
    __MODULE__
    |> where([e], e.items_instances_id == ^instance_id)
    |> preload([:ability, items_instances: :item])
    |> Repo.all
    |> Enum.reduce([], fn
         %{ability: ability, items_instances: %{item: %{attacks_per_round: attacks}}}, damages ->
           case AbilityDamageType.load_damage(ability.id) do
             [] ->
               damages
             damage ->
               damage =
                 Enum.map damage, fn damage ->
                   update_in(damage, [:potency], &(&1 / attacks))
                 end
               damages ++ damage
           end
       end)
  end

end
