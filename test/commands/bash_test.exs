defmodule ApathyDrive.Commands.BashTest do
  use ApathyDrive.ChannelCase
  alias ApathyDrive.Commands

  setup do
    {:ok, mobile: test_mobile()}
  end

  test "bashing without providing a direction", %{mobile: mobile} do
    Commands.Bash.execute(mobile, [])
    assert_push "scroll", %{html: "<p>Bash what?</p>"}
  end
end
