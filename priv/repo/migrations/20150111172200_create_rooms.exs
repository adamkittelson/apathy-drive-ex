defmodule ApathyDrive.Repo.Migrations.CreateRooms do
  use Ecto.Migration

  def up do
    create table(:rooms) do
      add(:legacy_id, :text)
      add(:name, :text)
      add(:keywords, {:array, :string})
      add(:description, :text)
      add(:light, :integer)
      add(:item_descriptions, :jsonb)
      add(:lair_size, :integer)
      add(:lair_monsters, {:array, :integer})
      add(:lair_frequency, :integer)
      add(:permanent_npc, :integer)
      add(:room_ability, :text)
      add(:start_room, :boolean)
      add(:trainable_skills, {:array, :string})
      add(:exits, :jsonb)

      timestamps
    end

    create(index(:rooms, [:legacy_id]))
  end

  def down do
    drop(table(:rooms))
  end
end
