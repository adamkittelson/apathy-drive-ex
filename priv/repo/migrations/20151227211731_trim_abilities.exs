defmodule ApathyDrive.Repo.Migrations.TrimAbilities do
  use Ecto.Migration

  def change do
    alter table(:abilities) do
      remove(:level)
      remove(:faction)
      remove(:command)
      remove(:name)
      remove(:kind)
      remove(:description)
      remove(:flags)
      remove(:global_cooldown)
    end
  end
end
