defmodule ApathyDrive.Repo.Migrations.AddExpBufferLastDrainedAtToCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      add(:exp_buffer_last_drained_at, :utc_datetime_usec)
    end
  end
end
