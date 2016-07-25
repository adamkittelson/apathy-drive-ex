defmodule ApathyDrive.Commands.BashTest do
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

  test "bashing while not possessing a monster", %{room: room} do
    Commands.Bash.execute(room, room.mobiles[:spirit_1], [])
    assert_receive {:scroll, "<p>You need a body to bash doors open, however, given that you don't have a body, you can simply pass right through them.</p>"}
  end
end
