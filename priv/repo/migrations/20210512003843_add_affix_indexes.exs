defmodule ApathyDrive.Repo.Migrations.AddAffixIndexes do
  use Ecto.Migration

  def change do
    create(index(:affixes_traits, :affix_id))
    create(index(:affixes_traits, :trait_id))
    create(index(:items_instances_affixes_traits, :item_instance_id))
    create(index(:items_instances_affixes_traits, :affix_trait_id))

    create(index(:affixes_items_types, :affix_id))
    create(index(:affixes_items_types, :item_type_id))
    create(index(:item_type_parents, :item_type_id))
    create(index(:item_type_parents, :parent_id))
    create(index(:items, :type_id))
    create(index(:trainers, :room_id))
    create(index(:trainers, :skill_id))

    create(index(:skill_tags, :tag_id))
    create(index(:skill_tags, :skill_id))

    create(index(:affix_tags, :tag_id))
    create(index(:affix_tags, :affix_id))

    create(index(:affix_skills, :skill_id))
    create(index(:affix_skills, :affix_id))

    create(index(:items_instances_affix_skills, :item_instance_id))
    create(index(:items_instances_affix_skills, :affix_skill_id))
  end
end
