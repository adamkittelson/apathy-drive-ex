defmodule ApathyDrive.Repo.Migrations.AddFatalToLimbSetLimbs do
  use Ecto.Migration

  def change do
    alter table(:limb_set_limbs) do
      add(:fatal, :boolean, default: false)
    end
  end
end
