defmodule ApathyDrive.Repo.Migrations.CreateTitles do
  use Ecto.Migration

  def change do
    create table(:titles) do
      add(:class_id, references(:classes, on_delete: :delete_all))
      add(:level, :integer)
      add(:text, :text)
    end
  end
end
