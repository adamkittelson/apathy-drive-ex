defmodule ApathyDrive.Scripts.ImpactCrit do
  alias ApathyDrive.{Ability, DamageType, Repo, Room}

  def execute(%Room{} = room, mobile_ref, target_ref) do
    mobile = room.mobiles[mobile_ref]
    target = room.mobiles[target_ref]

    if mobile && target do
      table = "Impact"

      letter = Enum.random(["B", "C", "D"])

      table_id = Repo.get_by(DamageType, name: table).id

      ability = Ability.critical_ability(table_id, letter)

      if ability do
        Ability.apply_ability(room, mobile, target, ability)
      else
        room
      end
    else
      room
    end
  end
end
