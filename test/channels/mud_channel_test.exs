defmodule ApathyDrive.MUDChannelTest do
  use ApathyDrive.ChannelCase

  setup do
    {:ok, mobile: test_mobile()}
  end

  # test "ping replies with status ok", %{socket: socket} do
  #   ref = push socket, "ping", %{"hello" => "there"}
  #   assert_reply ref, :ok, %{"hello" => "there"}
  # end
  #
  # test "shout broadcasts to mud:lobby", %{socket: socket} do
  #   push socket, "shout", %{"hello" => "all"}
  #   assert_broadcast "shout", %{"hello" => "all"}
  # end
  #
  test "broadcasts are pushed to the client", %{mobile: mobile} do
    broadcast_from! mobile.socket, "broadcast", %{"some" => "data"}
    assert_push "broadcast", %{"some" => "data"}
  end
end
