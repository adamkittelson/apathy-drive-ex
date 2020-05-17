defmodule ApathyDrive.ItemAdmin do
  def index(_) do
    [
      id: nil,
      name: nil,
      type: nil,
      worn_on: nil
    ]
  end

  # field(:name, :string)
  # field(:type, :string)
  # field(:worn_on, :string)

  # field(:armour_type, :string)
  # field(:game_limit, :integer)
  # field(:weight, :integer)
  # field(:speed, :integer)
  # field(:max_uses, :integer)
  # field(:getable, :boolean)
  # field(:droppable, :boolean)
  # field(:destroy_on_death, :boolean)
  # field(:destroy_when_fully_used, :boolean)
  # field(:robbable, :boolean)
  # field(:cost_value, :integer)
  # field(:cost_currency, :string)
  # field(:min_damage, :integer)
  # field(:max_damage, :integer)
  # field(:description, :string)
  # field(:hit_verbs, ApathyDrive.JSONB)
  # field(:miss_verbs, ApathyDrive.JSONB)
  # field(:destruct_message, :string)
  # field(:room_destruct_message, :string)
  # field(:global_drop_rarity, :string)
  # field(:level, :integer)

  # status: %{choices: [{"Publish", "publish"}, {"Pending", "pending"}]},
  # body: %{type: :textarea, rows: 4},
  # views: %{permission: :read},
  # settings: %{label: "Post Settings"}

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
