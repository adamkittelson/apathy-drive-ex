defmodule ApathyDrive.Repo.Migrations.Unities do
  use Ecto.Migration

  def change do
    alter table(:mobiles) do
      remove :unity
      add :unities, {:array, :string}
    end

    alter table(:spirits) do
      remove :unity
      add :unities, {:array, :string}
    end

    alter table(:room_unities) do
      remove :unity
    end
  end
end
