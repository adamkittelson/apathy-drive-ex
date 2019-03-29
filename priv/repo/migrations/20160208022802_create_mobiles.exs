defmodule ApathyDrive.Repo.Migrations.CreateMonsters do
  use Ecto.Migration

  def change do
    create table(:mobiles) do
      add(:room_id, references(:rooms))
      add(:monster_template_id, references(:monster_templates))

      add(:name, :text)
      add(:alignment, :text)
      add(:enter_message, :text)
      add(:exit_message, :text)
      add(:death_message, :text)
      add(:description, :text)
      add(:gender, :text)
      add(:greeting, :text)
      add(:level, :integer)
      add(:flags, {:array, :string})
      add(:experience, :integer)
      add(:auto_attack_interval, :float)
      add(:questions, :jsonb)

      timestamps()
    end
  end
end
