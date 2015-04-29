defmodule Commands.ScoreTest do
  use ExUnit.Case
  use ShouldI
  import ApathyDrive.Matchers

  with "a good spirit" do
    setup context do
      Dict.put(context, :spirit, %Spirit{name: "Adam",
                                         alignment: "good",
                                         level: 5,
                                         experience: 98765,
                                         socket: %Phoenix.Socket{transport_pid: self, joined: true}})
    end

    should("display status with a good name", context) do
      Commands.Score.execute(context.spirit, [])

      assert_adds_to_scroll "<p><span class='dark-green'>Name:</span> <span class='white'>Adam        </span> <span class='dark-green'>Experience:</span> <span class='dark-cyan'>98765</span></p>"
      assert_adds_to_scroll "<p><span class='dark-green'>Level:</span> <span class='dark-cyan'>5           </span></p>"
    end
  end

  with "a neutral spirit" do
    setup context do
      Dict.put(context, :spirit, %Spirit{name: "Adam",
                                         alignment: "neutral",
                                         level: 5,
                                         experience: 98765,
                                         socket: %Phoenix.Socket{transport_pid: self, joined: true}})
    end

    should("display status with a neutral name", context) do
      Commands.Score.execute(context.spirit, [])

      assert_adds_to_scroll "<p><span class='dark-green'>Name:</span> <span class='dark-cyan'>Adam        </span> <span class='dark-green'>Experience:</span> <span class='dark-cyan'>98765</span></p>"
      assert_adds_to_scroll "<p><span class='dark-green'>Level:</span> <span class='dark-cyan'>5           </span></p>"
    end
  end

  with "an evil spirit" do
    setup context do
      Dict.put(context, :spirit, %Spirit{name: "Adam",
                                         alignment: "evil",
                                         level: 5,
                                         experience: 98765,
                                         socket: %Phoenix.Socket{transport_pid: self, joined: true}})
    end

    should("display status with an evil name", context) do
      Commands.Score.execute(context.spirit, [])

      assert_adds_to_scroll "<p><span class='dark-green'>Name:</span> <span class='magenta'>Adam        </span> <span class='dark-green'>Experience:</span> <span class='dark-cyan'>98765</span></p>"
      assert_adds_to_scroll "<p><span class='dark-green'>Level:</span> <span class='dark-cyan'>5           </span></p>"
    end
  end

end
