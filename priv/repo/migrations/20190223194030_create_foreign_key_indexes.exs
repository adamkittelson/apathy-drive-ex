defmodule ApathyDrive.Repo.Migrations.CreateForeignKeyIndexes do
  use Ecto.Migration

  def up do
    create(index(:abilities_attributes, :ability_id))
    create(index(:abilities_attributes, :attribute_id))

    create(index(:abilities_damage_types, :ability_id))
    create(index(:abilities_damage_types, :damage_type_id))

    create(index(:abilities_resistances, :ability_id))
    create(index(:abilities_resistances, :damage_type_id))

    create(index(:abilities_traits, :ability_id))
    create(index(:abilities_traits, :trait_id))

    create(index(:characters, :race_id))
    create(index(:characters, :class_id))

    create(index(:classes_abilities, :ability_id))
    create(index(:classes_abilities, :class_id))

    create(index(:classes_traits, :trait_id))
    create(index(:classes_traits, :class_id))

    create(index(:enchantments, :items_instances_id))
    create(index(:enchantments, :ability_id))

    create(index(:items_abilities, :ability_id))
    create(index(:items_abilities, :item_id))
    create(index(:items_abilities, :type_id))

    create(index(:items_classes, :class_id))
    create(index(:items_classes, :item_id))

    create(index(:items_instances, :item_id))
    create(index(:items_instances, :character_id))
    create(index(:items_instances, :room_id))
    create(index(:items_instances, :shop_id))
    create(index(:items_instances, :dropped_for_character_id))

    create(index(:items_races, :race_id))
    create(index(:items_races, :item_id))

    create(index(:items_traits, :trait_id))
    create(index(:items_traits, :item_id))

    create(index(:lair_monsters, :room_id))
    create(index(:lair_monsters, :monster_id))

    create(index(:monsters_abilities, :ability_id))
    create(index(:monsters_abilities, :monster_id))

    create(index(:monsters_items, :item_id))
    create(index(:monsters_items, :monster_id))

    create(index(:monsters_resistances, :damage_type_id))
    create(index(:monsters_resistances, :monster_id))

    create(index(:monsters_traits, :trait_id))
    create(index(:monsters_traits, :monster_id))

    create(index(:races_resistances, :damage_type_id))
    create(index(:races_resistances, :race_id))

    create(index(:races_traits, :trait_id))
    create(index(:races_traits, :race_id))

    create(index(:rooms, :ability_id))
    create(index(:rooms, :zone_controller_id))

    create(index(:rooms_exits, :exit_id))
    create(index(:rooms_exits, :room_id))
    create(index(:rooms_exits, :destination_id))
    create(index(:rooms_exits, :item_id))
    create(index(:rooms_exits, :class_id))
    create(index(:rooms_exits, :race_id))
    create(index(:rooms_exits, :pre_move_ability_id))
    create(index(:rooms_exits, :post_move_ability_id))
    create(index(:rooms_exits, :ability_id))

    create(index(:rooms_monsters, :room_id))
    create(index(:rooms_monsters, :monster_id))
    create(index(:rooms_monsters, :zone_spawned_at))

    create(index(:rooms_placed_items, :item_id))
    create(index(:rooms_placed_items, :room_id))

    create(index(:shop_items, :item_id))
    create(index(:shop_items, :shop_id))

    create(index(:shops, :room_id))

    create(index(:titles, :class_id))
  end

  def down do
    drop(index(:abilities_attributes, :ability_id))
    drop(index(:abilities_attributes, :attribute_id))

    drop(index(:abilities_damage_types, :ability_id))
    drop(index(:abilities_damage_types, :damage_type_id))

    drop(index(:abilities_resistances, :ability_id))
    drop(index(:abilities_resistances, :damage_type_id))

    drop(index(:abilities_traits, :ability_id))
    drop(index(:abilities_traits, :trait_id))

    drop(index(:characters, :race_id))
    drop(index(:characters, :class_id))

    drop(index(:classes_abilities, :ability_id))
    drop(index(:classes_abilities, :class_id))

    drop(index(:classes_traits, :trait_id))
    drop(index(:classes_traits, :class_id))

    drop(index(:enchantments, :items_instances_id))
    drop(index(:enchantments, :ability_id))

    drop(index(:items_abilities, :ability_id))
    drop(index(:items_abilities, :item_id))
    drop(index(:items_abilities, :type_id))

    drop(index(:items_classes, :class_id))
    drop(index(:items_classes, :item_id))

    drop(index(:items_instances, :item_id))
    drop(index(:items_instances, :character_id))
    drop(index(:items_instances, :room_id))
    drop(index(:items_instances, :shop_id))
    drop(index(:items_instances, :dropped_for_character_id))

    drop(index(:items_races, :race_id))
    drop(index(:items_races, :item_id))

    drop(index(:items_traits, :trait_id))
    drop(index(:items_traits, :item_id))

    drop(index(:lair_monsters, :room_id))
    drop(index(:lair_monsters, :monster_id))

    drop(index(:monsters_abilities, :ability_id))
    drop(index(:monsters_abilities, :monster_id))

    drop(index(:monsters_items, :item_id))
    drop(index(:monsters_items, :monster_id))

    drop(index(:monsters_resistances, :damage_type_id))
    drop(index(:monsters_resistances, :monster_id))

    drop(index(:monsters_traits, :trait_id))
    drop(index(:monsters_traits, :monster_id))

    drop(index(:races_resistances, :damage_type_id))
    drop(index(:races_resistances, :race_id))

    drop(index(:races_traits, :trait_id))
    drop(index(:races_traits, :race_id))

    drop(index(:rooms, :ability_id))
    drop(index(:rooms, :zone_controller_id))

    drop(index(:rooms_exits, :exit_id))
    drop(index(:rooms_exits, :room_id))
    drop(index(:rooms_exits, :destination_id))
    drop(index(:rooms_exits, :item_id))
    drop(index(:rooms_exits, :class_id))
    drop(index(:rooms_exits, :race_id))
    drop(index(:rooms_exits, :pre_move_ability_id))
    drop(index(:rooms_exits, :post_move_ability_id))
    drop(index(:rooms_exits, :ability_id))

    drop(index(:rooms_monsters, :room_id))
    drop(index(:rooms_monsters, :monster_id))
    drop(index(:rooms_monsters, :zone_spawned_at))

    drop(index(:rooms_placed_items, :item_id))
    drop(index(:rooms_placed_items, :room_id))

    drop(index(:shop_items, :item_id))
    drop(index(:shop_items, :shop_id))

    drop(index(:shops, :room_id))

    drop(index(:titles, :class_id))
  end
end
