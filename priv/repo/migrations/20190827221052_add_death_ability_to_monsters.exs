defmodule ApathyDrive.Repo.Migrations.AddDeathAbilityToMonsters do
  use Ecto.Migration

  def change do
    alter table(:monsters) do
      add(:death_ability_id, references(:abilities, on_delete: :nilify_all))
    end
  end
end
