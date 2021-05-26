defmodule ApathyDrive.Repo.Migrations.AddRequiredAgilityToItems do
  use Ecto.Migration

  def change do
    alter table(:items) do
      add(:required_agi, :integer)
    end

    alter table(:items_instances) do
      add(:required_agi, :integer)
    end
  end
end
