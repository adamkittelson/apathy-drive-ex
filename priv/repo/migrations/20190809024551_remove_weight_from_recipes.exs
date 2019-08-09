defmodule ApathyDrive.Repo.Migrations.RemoveWeightFromRecipes do
  use Ecto.Migration

  def change do
    alter table(:crafting_recipes) do
      remove(:weight)
    end
  end
end
