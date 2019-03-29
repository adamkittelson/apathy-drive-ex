defmodule ApathyDrive.Repo.Migrations.MonsterTemplateChanges do
  use Ecto.Migration

  def up do
    alter table(:monster_templates) do
      add(:experience, :integer)
      remove(:strength)
      remove(:agility)
      remove(:intelligence)
      remove(:health)
      remove(:hit_verbs)
      remove(:damage)
      remove(:limbs)
    end
  end

  def down do
    alter table(:monster_templates) do
      remove(:experience)
      add(:strength, :integer)
      add(:agility, :integer)
      add(:intelligence, :integer)
      add(:health, :integer)
      add(:hit_verbs, {:array, :string})
      add(:damage, :jsonb)
      add(:limbs, :jsonb)
    end
  end
end
