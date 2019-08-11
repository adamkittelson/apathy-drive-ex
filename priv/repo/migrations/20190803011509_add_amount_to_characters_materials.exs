defmodule ApathyDrive.Repo.Migrations.AddAmountToCharactersMaterials do
  use Ecto.Migration

  def change do
    alter table(:characters_materials) do
      add(:amount, :integer)
    end
  end
end
