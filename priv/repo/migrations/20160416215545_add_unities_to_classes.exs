defmodule ApathyDrive.Repo.Migrations.AddUnitiesToClasses do
  use Ecto.Migration

  def change do
    alter table(:classes) do
      add(:unities, {:array, :string})
    end
  end
end
