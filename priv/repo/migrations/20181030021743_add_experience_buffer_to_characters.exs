defmodule ApathyDrive.Repo.Migrations.AddExperienceBufferToCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      add(:exp_buffer, :bigint, default: 0)
      add(:max_exp_buffer, :bigint, default: 0)
    end
  end
end
