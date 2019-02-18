defmodule ApathyDrive.Repo.Migrations.VisibleItemCleanup do
  use Ecto.Migration

  def change do
    alter table(:rooms_placed_items) do
      remove(:next_spawn_at)
    end
  end
end
