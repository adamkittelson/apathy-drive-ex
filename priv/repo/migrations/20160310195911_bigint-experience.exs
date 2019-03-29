defmodule :"Elixir.ApathyDrive.Repo.Migrations.Bigint-experience" do
  use Ecto.Migration

  def change do
    alter table(:mobiles) do
      modify(:experience, :bigint)
    end

    alter table(:spirits) do
      modify(:experience, :bigint)
    end
  end
end
