defmodule ApathyDrive.Repo.Migrations.CreateItemsInstancesAffixesTraits do
  use Ecto.Migration

  def change do
    create table(:items_instances_affixes_traits) do
      add(:item_instance_id, references(:items_instances, on_delete: :delete_all))
      add(:affix_trait_id, references(:affixes_traits, on_delete: :delete_all))

      add(:value, :jsonb)
      add(:description, :text)
    end
  end
end
