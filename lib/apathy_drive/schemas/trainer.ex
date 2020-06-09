defmodule ApathyDrive.Trainer do
  use ApathyDriveWeb, :model
  require Logger

  alias ApathyDrive.{Character, Mobile, Room, Trainer}

  schema "trainers" do
    field(:cost_multiplier, :float)
    field(:min_level, :integer)
    field(:max_level, :integer)

    has_many(:rooms, Room)
    belongs_to(:class, Room)
  end

  def trainer?(%Room{trainer: %Trainer{}}), do: true
  def trainer?(%Room{}), do: false

  def training_cost(%Trainer{}, %Character{level: 1}), do: 0

  def training_cost(%Trainer{} = trainer, %Character{level: level} = character) do
    charm = Mobile.attribute_at_level(character, :charm, character.level)

    next_level = level + 1
    charm_mod = 1 - (trunc(charm / 5.0) - 10) / 100
    trunc(next_level * 5 * (trainer.cost_multiplier + 1) * 10 * charm_mod)
  end
end
