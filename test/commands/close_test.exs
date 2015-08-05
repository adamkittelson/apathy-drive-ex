defmodule Commands.CloseTest do
  use ApathyDrive.ChannelCase

  setup do
    {:ok, spirit: test_spirit()}
  end

  test "receives an error message", %{spirit: spirit} do
    Commands.Close.execute(spirit, ["north"])
    assert_push "scroll", %{html: "<p>You need a body to do that.</p>"}
  end

end
