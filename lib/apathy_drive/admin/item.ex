defmodule ApathyDrive.ItemAdmin do
  def index(_) do
    [
      id: nil,
      name: nil,
      type: nil,
      worn_on: nil
    ]
  end

  def form_fields(_) do
    [
      id: nil,
      name: nil,
      type: nil,
      worn_on: nil,
      armour_type: nil,
      game_limit: nil,
      weight: nil,
      speed: nil,
      max_uses: nil,
      getable: nil,
      droppable: nil,
      destroy_on_death: nil,
      destroy_when_fully_used: nil,
      robbable: nil,
      cost_value: nil,
      cost_currency: nil,
      min_damage: nil,
      max_damage: nil,
      description: %{type: :textarea},
      # hit_verbs: %{type: {:array, :string}},
      # field(:miss_verbs, ApathyDrive.JSONB)
      destruct_message: nil,
      room_destruct_message: nil,
      global_drop_rarity: nil,
      level: nil
    ]
  end
end
