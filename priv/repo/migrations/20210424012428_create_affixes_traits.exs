defmodule ApathyDrive.Repo.Migrations.CreateAffixesTraits do
  use Ecto.Migration

  def change do
    create table(:affixes_traits) do
      add(:affix_id, references(:affixes, on_delete: :delete_all))
      add(:trait_id, references(:traits, on_delete: :delete_all))

      add(:value, :jsonb)
      add(:description, :text)
    end
  end
end
