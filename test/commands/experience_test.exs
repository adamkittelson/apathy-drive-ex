defmodule Commands.ExperienceTest do
  use ExUnit.Case
  use ShouldI
  import ApathyDrive.Matchers

  with "a level 1 spirit with no experience" do
    setup context do
      Dict.put(context, :spirit, %Spirit{level: 1,
                                         experience: 0,
                                         socket: %Phoenix.Socket{transport_pid: self, topic: "test", joined: true}})
    end

    should_add_to_scroll "<p><span class='dark-green'>Exp:</span> <span class='dark-cyan'>0</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>1</span> <span class='dark-green'>Exp needed for next level:</span> <span class='dark-cyan'>666 (666) [0%]</span></p>" do
      Commands.Experience.execute(context.spirit, [])
    end
  end

  with "a level 5 spirit with 12345 experience" do
    setup context do
      Dict.put(context, :spirit, %Spirit{level: 5,
                                         experience: 12345,
                                         socket: %Phoenix.Socket{transport_pid: self, topic: "test", joined: true}})
    end

    should_add_to_scroll "<p><span class='dark-green'>Exp:</span> <span class='dark-cyan'>12345</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>5</span> <span class='dark-green'>Exp needed for next level:</span> <span class='dark-cyan'>3296 (15641) [79%]</span></p>" do
      Commands.Experience.execute(context.spirit, [])
    end
  end

end
