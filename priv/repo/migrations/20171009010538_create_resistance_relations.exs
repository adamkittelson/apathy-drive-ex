defmodule ApathyDrive.Repo.Migrations.CreateResistanceRelations do
  use Ecto.Migration

  def change do
    create table(:monsters_resistances) do
      add :monster_id, references(:monsters, on_delete: :delete_all)
      add :damage_type_id, references(:damage_types, on_delete: :delete_all)
      add :amount, :integer
    end

    create table(:abilities_resistances) do
      add :ability_id, references(:abilities, on_delete: :delete_all)
      add :damage_type_id, references(:damage_types, on_delete: :delete_all)
      add :amount, :integer
    end

    create table(:races_resistances) do
      add :race_id, references(:races, on_delete: :delete_all)
      add :damage_type_id, references(:damage_types, on_delete: :delete_all)
      add :amount, :integer
    end
  end
end
