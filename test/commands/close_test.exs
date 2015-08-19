defmodule Commands.CloseTest do
  use ApathyDrive.ChannelCase

  setup do
    {:ok, mobile: test_mobile()}
  end

  # test "receives an error message", %{mobile: mobile} do
  #   Commands.Close.execute(mobile, ["north"])
  #   assert_push "scroll", %{html: "<p>You need a body to do that.</p>"}
  # end

end
