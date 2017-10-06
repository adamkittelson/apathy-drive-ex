defmodule ApathyDrive.ItemDamageType do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Item, DamageType}

  schema "items_damage_types" do
    field :kind, :string
    field :potency, :integer

    belongs_to :item, Item
    belongs_to :damage_type, DamageType
  end

  def load_damage(item_id, level) do
    __MODULE__
    |> where([mt], mt.item_id == ^item_id)
    |> preload([:damage_type, :item])
    |> Repo.all
    |> Enum.reduce([], fn
         %{damage_type: damage_type, kind: kind, potency: potency, item: %Item{rarity: rarity}}, damages ->
           [
             %{
               kind: kind,
               potency: potency_with_rarity_bonus(potency, rarity),
               damage_type: damage_type.name,
               level: level
              } | damages
           ]
       end)
  end

  def potency_with_rarity_bonus(potency, "common"),    do: potency * 0.8
  def potency_with_rarity_bonus(potency, "uncommon"),  do: potency * 0.9
  def potency_with_rarity_bonus(potency, "rare"),      do: potency
  def potency_with_rarity_bonus(potency, "epic"),      do: potency * 1.1
  def potency_with_rarity_bonus(potency, "legendary"), do: potency * 1.2

end
