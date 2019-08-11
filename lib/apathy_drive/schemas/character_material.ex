defmodule ApathyDrive.CharacterMaterial do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Character, Material}

  schema "characters_materials" do
    field(:amount, :integer)

    belongs_to(:character, Character)
    belongs_to(:material, Material)
  end

  def load_for_character(%Character{} = character) do
    materials =
      character
      |> Ecto.assoc(:characters_materials)
      |> Ecto.Query.preload(:material)
      |> Repo.all()
      |> Enum.reduce(%{}, fn %{amount: _amount, material: %{name: name}} = material, materials ->
        Map.put(materials, name, material)
      end)

    Map.put(character, :materials, materials)
  end
end
