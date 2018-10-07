defmodule ApathyDrive.Repo.Migrations.AddAbilitiesAttributes do
  use Ecto.Migration

  def change do
    create table(:attributes) do
      add(:name, :text)
      add(:description, :text)
    end

    create table(:abilities_attributes) do
      add(:ability_id, references(:abilities, on_delete: :delete_all))
      add(:attribute_id, references(:attributes, on_delete: :delete_all))
    end
  end
end
