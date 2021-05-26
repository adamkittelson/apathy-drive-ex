defmodule ApathyDrive.Repo.Migrations.CreateAffixes do
  use Ecto.Migration

  def change do
    create table(:affixes) do
      add(:type, :text)
      add(:name, :text)
      add(:spawnable, :boolean)
      add(:rare, :boolean)
      add(:level, :integer)
      add(:max_level, :integer)
      add(:required_level, :integer)
      add(:frequency, :integer)
      add(:group, :integer)

      timestamps()
    end
  end
end
