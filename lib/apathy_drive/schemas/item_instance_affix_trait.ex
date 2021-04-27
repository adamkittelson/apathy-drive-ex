defmodule ApathyDrive.ItemInstanceAffixTrait do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{AffixTrait, ItemInstance, ItemInstanceAffixTrait, Trait}

  schema "items_instances_affixes_traits" do
    belongs_to(:item_instance, ItemInstance)
    belongs_to(:affix_trait, AffixTrait)

    field(:value, ApathyDrive.JSONB)
    field(:description, :string)
  end

  def load_traits(nil, _item), do: %{}

  def load_traits(item_instance_id, item) do
    __MODULE__
    |> where([mt], mt.item_instance_id == ^item_instance_id)
    |> preload(affix_trait: [:trait])
    |> Repo.all()
    |> Enum.reduce(%{}, fn
      %ItemInstanceAffixTrait{value: value, affix_trait: %{trait: %{name: "AC%"}}}, abilities ->
        bonus = trunc(max(1, item.ac * (value / 100)))

        trait = %{"AC" => item.ac + bonus}
        Trait.merge_traits(abilities, trait)

      %ItemInstanceAffixTrait{value: value, affix_trait: %{trait: %{name: name}}}, abilities ->
        trait = %{name => value}
        Trait.merge_traits(abilities, trait)
    end)
    |> Map.put_new("AC", item.ac)
    |> Map.put("stack_key", "traits")
    |> Map.put("stack_count", 1)
  end

  def set_ac(item, effect) do
    if item.ac do
      ac_traits =
        Enum.reduce(item.affix_traits, %{}, fn trait, traits ->
          name = trait.affix_trait.trait.name

          if name == "AC%" do
            traits
            |> Map.put_new("AC%", 0)
            |> update_in(["AC%"], &(&1 + trait.value))
          else
            traits
          end
        end)

      ac =
        Enum.reduce(ac_traits, item.ac, fn
          {"AC%", value}, ac ->
            bonus = max(1, item.ac * (value / 100))
            ac + bonus

          _, ac ->
            ac
        end)

      IO.puts("#{item.name}, ac: #{item.ac}, with bonus: #{ac}")

      Map.put(effect, "AC", ac)
    else
      effect
    end
  end
end
