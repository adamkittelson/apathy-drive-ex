defmodule ApathyDrive.Scripts.DarkwoodForest do
  alias ApathyDrive.{Mobile, Room}
  import ApathyDrive.Scripts

  def execute(%Room{} = room, mobile_ref) do
    messages = [
      "A guardsman shouts out the time of day.",
      "A voice shouts aloud \"Read the bulletin in the Adventurer's Guild!\"",
      "Children rush past you hopping around in youthful glee.",
      "A cheer of many voices can be heard in the distance.",
      "The awful sound of a drunken chorus echoes through the streets.",
      "A dog barks off in the distance."
    ]

    if :rand.uniform(100) > 77 do
      message = Enum.random(messages)
    end

    roll = :rand.uniform(100)

    cond do
      roll <= 70 ->
        room

      roll <= 75 ->
        Mobile.send_scroll(
          room.mobiles[mobile_ref],
          "<p>An ominous wind blows through the trees.</p>"
        )

        room

      roll <= 84 ->
        Mobile.send_scroll(room.mobiles[mobile_ref], "<p>A flock of birds fly overhead.</p>")
        room

      roll <= 90 ->
        Mobile.send_scroll(
          room.mobiles[mobile_ref],
          "<p>The forest becomes strangely silent.</p>"
        )

        room

      roll <= 95 ->
        Mobile.send_scroll(
          room.mobiles[mobile_ref],
          "<p>The leaves begin to rustle, as if some beast were about to spring forth!</p>"
        )

        summon_monster(room)

      roll <= 100 ->
        Mobile.send_scroll(room.mobiles[mobile_ref], "<p>A dry twig snaps loudly behind you.</p>")
        summon_monster(room)
    end
  end

  defp summon_monster(room) do
    roll = :rand.uniform(100)

    cond do
      roll <= 70 ->
        room

      roll <= 80 ->
        # dark goblin archer
        summon(room, 48)

      roll <= 90 ->
        # wild dog
        summon(room, 50)

      roll <= 100 ->
        # bandit
        summon(room, 49)
    end
  end
end
