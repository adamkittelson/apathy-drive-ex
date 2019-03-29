defmodule ApathyDrive.Repo.Migrations.CreateMonsters do
  use Ecto.Migration

  def change do
    create table(:monsters) do
      add(:name, :text)
      add(:gender, :text)
      add(:grade, :text)
      add(:hostile, :boolean)
      add(:movement, :text)
      add(:chance_to_follow, :integer)
      add(:description, :text)
      add(:enter_message, :text)
      add(:exit_message, :text)
      add(:death_message, :text)
      add(:adjectives, :jsonb)

      timestamps
    end
  end
end
