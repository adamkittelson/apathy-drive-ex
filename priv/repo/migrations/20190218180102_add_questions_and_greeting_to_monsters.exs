defmodule ApathyDrive.Repo.Migrations.AddQuestionsAndGreetingToMonsters do
  use Ecto.Migration

  def change do
    alter table(:monsters) do
      add(:greeting, :text)
      add(:questions, :jsonb)
    end
  end
end
