defmodule ApathyDrive.Repo.Migrations.AddSlots do
  use Ecto.Migration

  def up do
    create table(:slots) do
      add(:name, :text)
    end

    alter table(:limb_set_limbs) do
      remove(:slot)
    end

    create table(:limb_set_limb_slots) do
      add(:slot_id, references(:slots, on_delete: :delete_all))
      add(:limb_set_limb_id, references(:limb_set_limbs, on_delete: :delete_all))
    end

    create(index(:limb_set_limb_slots, [:slot_id]))
    create(index(:limb_set_limb_slots, [:limb_set_limb_id]))
  end

  def down do
    drop(index(:limb_set_limb_slots, [:slot_id]))
    drop(constraint(:limb_set_limb_slots, "limb_set_limb_slots_slot_id_fkey"))

    drop(table(:slots))

    alter table(:limb_set_limbs) do
      add(:slot, :text)
    end

    drop(table(:limb_set_limb_slots))
  end
end
