defmodule ApathyDrive.Commands.ExperienceTest do
  use ApathyDrive.ChannelCase
  alias ApathyDrive.Commands

  setup do
    room =
      %Room{
        mobiles: %{
          spirit_1: %Mobile{level: 1, experience: 0, ref: :spirit_1, socket: self},
          spirit_2: %Mobile{level: 5, experience: 12345, ref: :spirit_1, socket: self}
        }
      }

    {:ok, room: room}
  end

  test "a level 1 spirit with no experience", %{room: room} do
    Commands.Experience.execute(room, room.mobiles[:spirit_1], [])
    assert_receive {:scroll, "<p><span class='dark-green'>Essence:</span> <span class='dark-cyan'>0</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>1</span> <span class='dark-green'>Essence needed for next level:</span> <span class='dark-cyan'>666 (666) [0%]</span></p>"}
  end

  test "a level 5 spirit with 12345 experience", %{room: room} do
    Commands.Experience.execute(room, room.mobiles[:spirit_2], [])
    assert_receive {:scroll, "<p><span class='dark-green'>Essence:</span> <span class='dark-cyan'>12345</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>5</span> <span class='dark-green'>Essence needed for next level:</span> <span class='dark-cyan'>3296 (15641) [79%]</span></p>"}
  end
end
