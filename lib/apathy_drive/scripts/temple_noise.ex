defmodule ApathyDrive.Scripts.TempleNoise do
  alias ApathyDrive.{Mobile, Room}

  def execute(%Room{} = room, mobile_ref) do
    messages = [
      "The smell of incense wafts faintly in the air.",
      "The angelic sound of a choir floats down through the air.",
      "The large temple bell clangs loudly, echoing the time of day."
    ]

    if :rand.uniform(100) > 97 do
      message = Enum.random(messages)
      Mobile.send_scroll(room.mobiles[mobile_ref], "<p>#{message}</p>")
    end

    room
  end
end
