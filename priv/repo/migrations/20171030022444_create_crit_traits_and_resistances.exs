defmodule ApathyDrive.Repo.Migrations.CreateCritTraitsAndResistances do
  use Ecto.Migration

  def change do
    create table(:crits_traits) do
      add(:crit_id, references(:crits, on_delete: :delete_all))
      add(:trait_id, references(:traits, on_delete: :delete_all))
      add(:value, :jsonb)
    end

    create table(:crits_resistances) do
      add(:crit_id, references(:crits, on_delete: :delete_all))
      add(:damage_type_id, references(:damage_types, on_delete: :delete_all))
      add(:amount, :integer)
    end

    create(index(:crits_traits, [:crit_id, :trait_id], unique: true))
  end
end
