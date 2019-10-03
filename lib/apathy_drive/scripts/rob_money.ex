defmodule ApathyDrive.Scripts.RobMoney do
  alias ApathyDrive.{Character, Repo, Room}

  def execute(%Room{} = room, mobile_ref, target_ref) do
    mobile = room.mobiles[mobile_ref]
    target = room.mobiles[target_ref]

    if mobile && target do
      mobile =
        if mobile.__struct__ == Character do
          mobile
          |> Ecto.Changeset.change(%{
            runic: mobile.runic + target.runic,
            platinum: mobile.platinum + target.platinum,
            gold: mobile.gold + target.gold,
            silver: mobile.silver + target.silver,
            copper: mobile.copper + target.copper
          })
          |> Repo.update!()
        else
          mobile
          |> Map.merge(%{
            runic: mobile.runic + target.runic,
            platinum: mobile.platinum + target.platinum,
            gold: mobile.gold + target.gold,
            silver: mobile.silver + target.silver,
            copper: mobile.copper + target.copper
          })
        end

      target =
        if target.__struct__ == Character do
          target
          |> Ecto.Changeset.change(%{
            runic: 0,
            platinum: 0,
            gold: 0,
            silver: 0,
            copper: 0
          })
          |> Repo.update!()
        else
          target
          |> Map.merge(%{
            runic: 0,
            platinum: 0,
            gold: 0,
            silver: 0,
            copper: 0
          })
        end

      room
      |> put_in([:mobiles, mobile_ref], mobile)
      |> put_in([:mobiles, target_ref], target)
    else
      room
    end
  end
end
