defmodule ApathyDrive.Repo.Migrations.AddLimbs do
  use Ecto.Migration

  def change do
    create table(:limbs) do
      # e.g. "arm", "hand"
      add(:type, :text)
    end

    create table(:limb_sets) do
      # e.g. "humanoid", "beast", "dragon", "angel"
      add(:name, :text)
    end

    create table(:limb_set_limbs) do
      add(:limb_set_id, references(:limb_sets, on_delete: :delete_all))
      add(:limb_id, references(:limbs, on_delete: :delete_all))
      # e.g. "left", "right"
      add(:location, :text)
      add(:depends_on, references(:limb_set_limbs, on_delete: :delete_all))
      # e.g. Arms, Weapon Hand
      add(:slot, :text)
    end

    alter table(:races) do
      add(:limb_set_id, references(:limb_sets, on_delete: :delete_all))
    end

    alter table(:monsters) do
      add(:limb_set_id, references(:limb_sets, on_delete: :delete_all))
    end

    alter table(:characters) do
      add(:missing_limbs, {:array, :string})
    end

    alter table(:rooms_monsters) do
      add(:missing_limbs, {:array, :string})
    end

    create(index(:limb_set_limbs, [:limb_set_id]))
    create(index(:limb_set_limbs, [:limb_id]))
    create(index(:limb_set_limbs, [:depends_on]))
    create(index(:races, [:limb_set_id]))
    create(index(:monsters, [:limb_set_id]))
  end
end
