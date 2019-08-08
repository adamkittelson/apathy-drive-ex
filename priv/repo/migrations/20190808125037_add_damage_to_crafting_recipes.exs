defmodule ApathyDrive.Repo.Migrations.AddDamageToCraftingRecipes do
  use Ecto.Migration

  def change do
    alter table(:crafting_recipes) do
      add(:damage, :integer)
      remove(:speed)
    end

    alter table(:items) do
      remove(:required_strength)
    end
  end
end
