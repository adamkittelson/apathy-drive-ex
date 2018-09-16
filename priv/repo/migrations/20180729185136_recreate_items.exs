defmodule ApathyDrive.Repo.Migrations.RecreateItems do
  use Ecto.Migration

  def change do
    drop(table(:shop_items))

    alter table(:items) do
      remove(:name)
      remove(:description)
      remove(:worn_on)
      remove(:inserted_at)
      remove(:updated_at)
      remove(:grade)
      remove(:global_drop)
      remove(:game_limit)
      remove(:rarity)
      remove(:hit_verbs)
      remove(:miss_verbs)
      remove(:attacks_per_round)
      remove(:physical_resistance)
      remove(:magical_resistance)
      remove(:kind)
      remove(:speed)
      remove(:weight)
      remove(:min_damage)
      remove(:max_damage)
      remove(:required_strength)
      remove(:required_agility)
      remove(:required_intellect)
      remove(:required_willpower)
      remove(:required_health)
      remove(:required_charm)

      add(:name, :text)
      add(:type, :text)
      add(:worn_on, :text)
      add(:weapon_type, :text)
      add(:armour_type, :text)
      add(:game_limit, :integer)
      add(:weight, :integer)
      add(:speed, :integer)
      add(:required_strength, :integer)
      add(:max_uses, :integer)
      add(:getable, :boolean)
      add(:droppable, :boolean)
      add(:destroy_on_death, :boolean)
      add(:destroy_when_fully_used, :boolean)
      add(:robbable, :boolean)
      add(:cost_value, :integer)
      add(:cost_currency, :text)
      add(:min_damage, :integer)
      add(:max_damage, :integer)
      add(:description, :text)
      add(:hit_verbs, :jsonb)
      add(:miss_verbs, :jsonb)
      add(:destruct_message, :text)
    end

    create table(:shop_items) do
      add(:room_id, references(:rooms, on_delete: :delete_all))
      add(:item_id, references(:items, on_delete: :delete_all))
      add(:stock, :integer)
      add(:max_stock, :integer)
      add(:regen_frequency_in_minutes, :integer)
      add(:regen_chance, :integer)
      add(:regen_amount, :integer)
      add(:next_regen_at, :utc_datetime)
    end

    create(unique_index(:shop_items, [:room_id, :item_id]))
  end
end
