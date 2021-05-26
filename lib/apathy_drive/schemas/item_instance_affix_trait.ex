defmodule ApathyDrive.ItemInstanceAffixTrait do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{AffixTrait, ItemInstance, ItemInstanceAffixTrait, Trait}

  schema "items_instances_affixes_traits" do
    belongs_to(:item_instance, ItemInstance)
    belongs_to(:affix_trait, AffixTrait)

    field(:value, ApathyDrive.JSONB)
    field(:description, :string)
  end

  def affix_groups_on_item(item_instance, affix_type) do
    __MODULE__
    |> Ecto.Query.where([iiat], iiat.item_instance_id == ^item_instance.id)
    |> Ecto.Query.preload(affix_trait: [:affix])
    |> Repo.all()
    |> Enum.filter(&(&1.affix_trait.affix.type == affix_type))
    |> Enum.map(& &1.affix_trait.affix.group)
  end

  def load_traits(nil, _item), do: %{}

  def load_traits(item_instance_id, item) do
    __MODULE__
    |> where([mt], mt.item_instance_id == ^item_instance_id)
    |> preload(affix_trait: [:trait])
    |> Repo.all()
    |> Enum.reduce(%{}, fn
      %ItemInstanceAffixTrait{value: value, affix_trait: %{trait: %{name: "Defense%"}}},
      abilities ->
        bonus = trunc(max(1, (item.ac || 0) * (value / 100)))

        trait = %{"Defense" => (item.ac || 0) + bonus}
        Trait.merge_traits(abilities, trait)

      %ItemInstanceAffixTrait{value: value, affix_trait: %{trait: %{name: "Defense"}}},
      abilities ->
        trait = %{"Defense" => (item.ac || 0) + value}
        Trait.merge_traits(abilities, trait)

      %ItemInstanceAffixTrait{value: value, affix_trait: %{trait: %{name: name}}}, abilities ->
        trait = %{name => value}
        Trait.merge_traits(abilities, trait)
    end)
    |> Map.put_new("Defense", item.ac || 0)
    |> Map.put("stack_key", "traits")
    |> Map.put("stack_count", 1)
  end
end
