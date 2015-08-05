defmodule Commands.ScoreTest do
  use ApathyDrive.ChannelCase

  setup do
    {:ok, good: test_spirit(%{name: "Adam",
                              alignment: "good",
                              level: 5,
                              experience: 98765}),
          neutral: test_spirit(%{name: "Adam",
                                 alignment: "neutral",
                                 level: 5,
                                 experience: 98765}),
          evil: test_spirit(%{name: "Adam",
                              alignment: "evil",
                              level: 5,
                              experience: 98765})}
  end

  test "a good spirit", %{good: spirit} do
    Commands.Score.execute(spirit, [])
    assert_push "scroll", %{html: "<p><span class='dark-green'>Name:</span> <span class='white'>Adam        </span> <span class='dark-green'>Experience:</span> <span class='dark-cyan'>98765</span></p>"}
    assert_push "scroll", %{html: "<p><span class='dark-green'>Level:</span> <span class='dark-cyan'>5           </span></p>"}
  end

  test "a neutral spirit", %{neutral: spirit} do
    Commands.Score.execute(spirit, [])
    assert_push "scroll", %{html: "<p><span class='dark-green'>Name:</span> <span class='dark-cyan'>Adam        </span> <span class='dark-green'>Experience:</span> <span class='dark-cyan'>98765</span></p>"}
    assert_push "scroll", %{html: "<p><span class='dark-green'>Level:</span> <span class='dark-cyan'>5           </span></p>"}
  end

  test "an evil spirit", %{evil: spirit} do
    Commands.Score.execute(spirit, [])
    assert_push "scroll", %{html: "<p><span class='dark-green'>Name:</span> <span class='magenta'>Adam        </span> <span class='dark-green'>Experience:</span> <span class='dark-cyan'>98765</span></p>"}
    assert_push "scroll", %{html: "<p><span class='dark-green'>Level:</span> <span class='dark-cyan'>5           </span></p>"}
  end

end
