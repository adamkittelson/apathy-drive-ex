defmodule ApathyDrive.Repo.Migrations.CreateAbilityAttributes do
  use Ecto.Migration

  def change do
    create table(:abilities_attributes) do
      add(:ability_id, references(:abilities, on_delete: :delete_all))
      add(:attribute_id, references(:attributes, on_delete: :delete_all))
    end

    create(index(:abilities_attributes, :ability_id))
    create(index(:abilities_attributes, :attribute_id))
    create(index(:abilities_attributes, [:ability_id, :attribute_id], unique: true))
  end
end
