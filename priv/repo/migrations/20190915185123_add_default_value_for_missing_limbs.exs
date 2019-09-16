defmodule ApathyDrive.Repo.Migrations.AddDefaultValueForMissingLimbs do
  use Ecto.Migration

  def change do
    alter table(:characters) do
      modify(:missing_limbs, {:array, :string}, default: [])
    end

    alter table(:rooms_monsters) do
      modify(:missing_limbs, {:array, :string}, default: [])
    end
  end
end
