defmodule ApathyDrive.Repo.Migrations.MoveCraftingMoneyToMaterials do
  use Ecto.Migration

  def change do
    alter table(:crafting_recipes) do
      remove(:cost_value)
      remove(:cost_currency)
    end

    alter table(:materials) do
      add(:cost_value, :integer)
      add(:cost_currency, :string)
    end
  end
end
