defmodule ApathyDrive.Scripts.ElementalControl do
  alias ApathyDrive.{Ability, DamageType, Mobile, Repo, Room}

  @tables ["Vacuum", "Electricity", "Fire", "Cold", "Holy"]

  def execute(%Room{} = room, mobile_ref, target_ref) do
    mobile = room.mobiles[mobile_ref]
    target = room.mobiles[target_ref]

    if mobile && target do
      table = Enum.random(@tables)

      Mobile.send_scroll(mobile, "<p>You use <span class='yellow'>#{table}</span>.</p>")

      Room.send_scroll(room, "<p>#{mobile.name} uses <span class='yellow'>#{table}</span>.</p>", [
        mobile
      ])

      letter = Enum.random(["B", "C", "D", "E"])

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
