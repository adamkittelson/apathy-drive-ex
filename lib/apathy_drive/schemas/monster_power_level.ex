defmodule ApathyDrive.MonsterPowerLevel do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Monster, PowerLevel}

  schema "monsters_power_levels" do
    belongs_to(:monster, Monster)
    belongs_to(:power_level, PowerLevel)
  end

  def load(%Monster{id: id} = monster) do
    mpl =
      Repo.get_by(__MODULE__, monster_id: id)
      |> Repo.preload(:power_level)

    Map.put(monster, :power_level, mpl.power_level)
  end
end
