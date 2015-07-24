defmodule ApathyDrive.PlayerControllerTest do
  use ApathyDrive.ConnCase

  alias ApathyDrive.Player
  @valid_attrs %{email: "adam@apathydrive.com",
                 password: "awesome password",
                 password_confirmation: "awesome password"}
  @invalid_attrs %{}

  setup do
    conn = conn()
    {:ok, conn: conn}
  end

  test "creates resource and redirects when data is valid", %{conn: conn} do
    conn = post conn, player_path(conn, :create), player: @valid_attrs
    assert redirected_to(conn) == character_path(conn, :index)
    assert Repo.get_by(Player, %{email: @valid_attrs.email})
  end

  test "does not create resource and renders errors when data is invalid", %{conn: conn} do
    conn = post conn, player_path(conn, :create), player: @invalid_attrs
    assert html_response(conn, 200) =~ "Sign Up"
  end

end
