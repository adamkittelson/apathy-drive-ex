defmodule ApathyDrive.CraftingRecipeTrait do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{CraftingRecipe, CraftingRecipeTrait, Trait}

  schema "crafting_recipes_traits" do
    field(:value, ApathyDrive.JSONB)
    field(:delete, :boolean, virtual: true)

    belongs_to(:crafting_recipe, CraftingRecipe)
    belongs_to(:trait, Trait)
  end

  @required_fields ~w(trait_id value)a

  def load_traits(crafting_recipe_id) do
    __MODULE__
    |> where([r], r.crafting_recipe_id == ^crafting_recipe_id)
    |> preload([:trait])
    |> Repo.all()
    |> Enum.reduce(%{}, fn
      %{trait: %{name: name}, value: value}, abilities ->
        trait = %{name => value}
        Trait.merge_traits(abilities, trait)
    end)
  end

  def changeset(%CraftingRecipeTrait{} = rt, attrs) do
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
