defmodule ApathyDrive.Repo.Migrations.RemoveQuestionsFromMonsters do
  use Ecto.Migration

  def change do
    alter table(:mobiles) do
      remove :questions
    end
  end
end
