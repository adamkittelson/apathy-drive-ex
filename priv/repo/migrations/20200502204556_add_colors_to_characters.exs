defmodule ApathyDrive.Repo.Migrations.AddColorsToCharacters do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      add(:attack_color, :text, default: "red")
      add(:target_color, :text, default: "red")
      add(:spectator_color, :text, default: "red")
    end
  end
end
