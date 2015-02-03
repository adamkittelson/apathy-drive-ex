defmodule Commands.ScoreTest do
  use ExUnit.Case
  use ShouldI
  import ApathyDrive.Matchers.Socket

  with "a spirit with no name" do
    setup context do
      Dict.put(context, :spirit, %Spirit{name: nil,
                                         level: 1,
                                         experience: 1234,
                                         socket: %Phoenix.Socket{pid: self}})
    end

    should("display anonymous status", context) do
      Commands.Score.execute(context.spirit, [])

      assert_adds_to_scroll "<p><span class='dark-green'>Name:</span> <span class='dark-cyan'>Anonymous   </span> <span class='dark-green'>Experience:</span> <span class='dark-cyan'>1234</span></p>"
      assert_adds_to_scroll "<p><span class='dark-green'>Level:</span> <span class='dark-cyan'>1</span></p>"
    end
  end

  with "a spirit with a name" do
    setup context do
      Dict.put(context, :spirit, %Spirit{name: "Adam",
                                         level: 5,
                                         experience: 98765,
                                         socket: %Phoenix.Socket{pid: self}})
    end

    should("display status with a name", context) do
      Commands.Score.execute(context.spirit, [])

      assert_adds_to_scroll "<p><span class='dark-green'>Name:</span> <span class='dark-cyan'>Adam        </span> <span class='dark-green'>Experience:</span> <span class='dark-cyan'>5</span></p>"
      assert_adds_to_scroll "<p><span class='dark-green'>Level:</span> <span class='dark-cyan'>5</span></p>"
    end
  end

end
