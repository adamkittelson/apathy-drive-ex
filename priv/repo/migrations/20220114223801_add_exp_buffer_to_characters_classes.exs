defmodule ApathyDrive.Repo.Migrations.AddExpBufferToCharactersClasses do
  use Ecto.Migration

  def change do
    alter table(:characters_classes) do
      add(:exp_buffer, :integer)
    end
  end
end
