defmodule ApathyDrive.Repo.Migrations.RemoveQuestionsFromMonsters do
  use Ecto.Migration

  def change do
    alter table(:monsters) do
      remove :questions
    end
  end
end
