defmodule ApathyDrive.Repo.Migrations.AddPityModifierToCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      add :pity_modifier, :integer, default: 0
    end
  end
end
