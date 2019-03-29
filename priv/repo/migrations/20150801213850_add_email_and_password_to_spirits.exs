defmodule ApathyDrive.Repo.Migrations.AddEmailAndPasswordToSpirits do
  use Ecto.Migration

  def change do
    alter table(:spirits) do
      add(:email, :text)
      add(:password, :text)
    end
  end
end
