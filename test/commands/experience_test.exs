defmodule ApathyDrive.Commands.ExperienceTest do
  use ApathyDrive.ChannelCase
  alias ApathyDrive.Commands

  setup do
    {:ok, spirit_1: test_mobile(%{level: 1, experience: 0}),
          spirit_2: test_mobile(%{level: 5, experience: 12345})}
  end

  test "a level 1 spirit with no experience", %{spirit_1: spirit} do
    Commands.Experience.execute(spirit, [])
    assert_push "scroll", %{html: "<p><span class='dark-green'>Essence:</span> <span class='dark-cyan'>0</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>1</span> <span class='dark-green'>Essence needed for next level:</span> <span class='dark-cyan'>666 (666) [0%]</span></p>"}
  end

  test "a level 5 spirit with 12345 experience", %{spirit_2: spirit} do
    Commands.Experience.execute(spirit, [])
    assert_push "scroll", %{html: "<p><span class='dark-green'>Essence:</span> <span class='dark-cyan'>12345</span> <span class='dark-green'>Level:</span> <span class='dark-cyan'>5</span> <span class='dark-green'>Essence needed for next level:</span> <span class='dark-cyan'>3296 (15641) [79%]</span></p>"}
  end
end
