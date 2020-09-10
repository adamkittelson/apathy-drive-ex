defmodule ApathyDrive.LootPity do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Character, MonsterItem}
  require Logger

  schema "characters_monsters_items" do
    field(:pity, :integer, default: 0)

    belongs_to(:character, Character)
    belongs_to(:monsters_items, MonsterItem)
  end

  def increase_pity(%Character{id: character_id} = character, loot_id) do
    pity =
      case Repo.get_by(__MODULE__, character_id: character_id, monsters_items_id: loot_id) do
        nil ->
          %__MODULE__{character_id: character_id, monsters_items_id: loot_id}

        %__MODULE__{} = loot_pity ->
          loot_pity
      end

    Logger.info(
      "Increasing MonsterItem##{loot_id} pity for #{character.name} to #{pity.pity + 1}"
    )

    pity
    |> Ecto.Changeset.change(%{pity: pity.pity + 1})
    |> Repo.insert_or_update!()
  end

  def pity_for_character(%Character{id: character_id} = character, loot_id) do
    case Repo.get_by(__MODULE__, character_id: character_id, monsters_items_id: loot_id) do
      nil ->
        0

      %__MODULE__{pity: pity} ->
        Logger.info("MonsterItem##{loot_id} pity for #{character.name}: #{pity}")
        pity
    end
  end

  def reset_pity(%Character{id: character_id} = character, loot_id) do
    case Repo.get_by(__MODULE__, character_id: character_id, monsters_items_id: loot_id) do
      nil ->
        :noop

      %__MODULE__{} = loot_pity ->
        Logger.info("Resetting MonsterItem##{loot_id} pity for #{character.name}")
        Repo.delete!(loot_pity)
    end
  end
end
