defmodule ApathyDrive.Scripts.TrollDeath do
  alias ApathyDrive.Room
  import ApathyDrive.Scripts

  def execute(%Room{} = room, _mobile_ref) do
    roll = :rand.uniform(100)

    if roll <= 25 do
      Room.send_scroll(
        room,
        "<p><span class='blue'>The swamp troll regenerates, and gets back up!"
      )

      summon(room, "swamp troll")
    else
      room
    end
  end
end
