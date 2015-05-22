defmodule ApathyDrive.RoomControllerTest do
  use ApathyDrive.ConnCase

  @valid_params room: %{}
  @invalid_params room: %{}

  setup do
    conn = conn()
    {:ok, conn: conn}
  end

  test "GET /rooms", %{conn: conn} do
    conn = get conn, room_path(conn, :index)
    assert html_response(conn, 200) =~ "<th>ID</th><th>Room</th><th>Actions</th>"
  end

  # test "GET /rooms/new", %{conn: conn} do
  #   conn = get conn, room_path(conn, :new)
  #   assert html_response(conn, 200) =~ "New room"
  # end
  # 
  # test "POST /rooms with valid data", %{conn: conn} do
  #   conn = post conn, room_path(conn, :create), @valid_params
  #   assert redirected_to(conn) == room_path(conn, :index)
  # end
  # 
  # test "POST /rooms with invalid data", %{conn: conn} do
  #   conn = post conn, room_path(conn, :create), @invalid_params
  #   assert html_response(conn, 200) =~ "New room"
  # end
  # 
  # test "GET /rooms/:id", %{conn: conn} do
  #   room = Repo.insert %Room{}
  #   conn = get conn, room_path(conn, :show, room)
  #   assert html_response(conn, 200) =~ "Show room"
  # end
  # 
  # test "GET /rooms/:id/edit", %{conn: conn} do
  #   room = Repo.insert %Room{}
  #   conn = get conn, room_path(conn, :edit, room)
  #   assert html_response(conn, 200) =~ "Edit room"
  # end
  # 
  # test "PUT /rooms/:id with valid data", %{conn: conn} do
  #   room = Repo.insert %Room{}
  #   conn = put conn, room_path(conn, :update, room), @valid_params
  #   assert redirected_to(conn) == room_path(conn, :index)
  # end
  # 
  # test "PUT /rooms/:id with invalid data", %{conn: conn} do
  #   room = Repo.insert %Room{}
  #   conn = put conn, room_path(conn, :update, room), @invalid_params
  #   assert html_response(conn, 200) =~ "Edit room"
  # end
  # 
  # test "DELETE /rooms/:id", %{conn: conn} do
  #   room = Repo.insert %Room{}
  #   conn = delete conn, room_path(conn, :delete, room)
  #   assert redirected_to(conn) == room_path(conn, :index)
  #   refute Repo.get(Room, room.id)
  # end
end
