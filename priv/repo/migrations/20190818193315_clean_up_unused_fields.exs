defmodule ApathyDrive.Repo.Migrations.CleanUpUnusedFields do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      remove(:strength_experience)
      remove(:agility_experience)
      remove(:intellect_experience)
      remove(:willpower_experience)
      remove(:health_experience)
      remove(:charm_experience)
      remove(:max_exp_buffer)
    end
  end
end
