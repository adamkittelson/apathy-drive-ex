defmodule ApathyDrive.Repo.Migrations.AddAcAndMaxSocketsToItems do
  use Ecto.Migration

  def change do
    alter table(:items) do
      add(:min_ac, :integer)
      add(:max_ac, :integer)
      add(:max_sockets, :integer)
      add(:required_str, :integer)
      add(:magic_level, :integer)
    end

    alter table(:items_instances) do
      add(:ac, :integer)
      add(:required_str, :integer)
    end
  end
end
