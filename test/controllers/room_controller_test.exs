defmodule ApathyDrive.RoomControllerTest do
  use ApathyDrive.ConnCase
  use ShouldI

  @valid_params room: %{"name" => "test room", "description" => "this is the description"}
  @invalid_params room: %{}

  with "admin privledges" do
    setup context do
      conn = conn()
             |> assign(:admin?, true)

      Dict.put context, :conn, conn
    end

    should "GET /rooms", context do
      conn = get context.conn, room_path(context.conn, :index)
      assert html_response(conn, 200) =~ "<th>ID</th><th>Room</th><th>Actions</th>"
    end

    should "GET /rooms/new", context do
      conn = get context.conn, room_path(context.conn, :new)
      assert html_response(conn, 200) =~ "New room"
    end

    should "POST /rooms with valid data", context do
      conn = post context.conn, room_path(context.conn, :create), @valid_params
      assert redirected_to(conn) == room_path(conn, :index)
    end

    should "POST /rooms with invalid data", context do
      conn = post context.conn, room_path(context.conn, :create), @invalid_params
      assert html_response(conn, 200) =~ "New room"
    end

    should "GET /rooms/:id", context do
      room = Repo.insert %Room{}
      conn = get context.conn, room_path(context.conn, :show, room)
      assert html_response(conn, 200) =~ "Show room"
    end

    should "GET /rooms/:id/edit", context do
      room = Repo.insert %Room{}
      conn = get context.conn, room_path(context.conn, :edit, room)
      assert html_response(conn, 200) =~ "Edit room"
    end

    should "PUT /rooms/:id with valid data", context do
      room = Repo.insert %Room{}
      conn = put context.conn, room_path(context.conn, :update, room), @valid_params
      assert redirected_to(conn) == room_path(conn, :index)
    end

    should "PUT /rooms/:id with invalid data", context do
      room = Repo.insert %Room{}
      conn = put context.conn, room_path(context.conn, :update, room), @invalid_params
      assert html_response(conn, 200) =~ "Edit room"
    end

    should "DELETE /rooms/:id", context do
      room = Repo.insert %Room{}
      conn = delete context.conn, room_path(context.conn, :delete, room)
      assert redirected_to(conn) == room_path(conn, :index)
      refute Repo.get(Room, room.id)
    end

  end

  with "no admin privledges" do
    setup context do
      conn = conn()

      Dict.put context, :conn, conn
    end

    should "GET /rooms should redirect to the home page", context do
      conn = get context.conn, room_path(context.conn, :index)
      assert redirected_to(conn) == page_path(conn, :index)
    end

    should "GET /rooms/new should redirect to the home page", context do
      conn = get context.conn, room_path(context.conn, :new)
      assert redirected_to(conn) == page_path(conn, :index)
    end

    should "POST /rooms with valid data should redirect to the home page", context do
      conn = post context.conn, room_path(context.conn, :create), @valid_params
      assert redirected_to(conn) == page_path(conn, :index)
    end

    should "POST /rooms with invalid data should redirect to the home page", context do
      conn = post context.conn, room_path(context.conn, :create), @invalid_params
      assert redirected_to(conn) == page_path(conn, :index)
    end

    should "GET /rooms/:id should redirect to the home page", context do
      room = Repo.insert %Room{}
      conn = get context.conn, room_path(context.conn, :show, room)
      assert redirected_to(conn) == page_path(conn, :index)
    end

    should "GET /rooms/:id/edit should redirect to the home page", context do
      room = Repo.insert %Room{}
      conn = get context.conn, room_path(context.conn, :edit, room)
      assert redirected_to(conn) == page_path(conn, :index)
    end

    should "PUT /rooms/:id with valid data should redirect to the home page", context do
      room = Repo.insert %Room{}
      conn = put context.conn, room_path(context.conn, :update, room), @valid_params
      assert redirected_to(conn) == page_path(conn, :index)
    end

    should "PUT /rooms/:id with invalid data should redirect to the home page", context do
      room = Repo.insert %Room{}
      conn = put context.conn, room_path(context.conn, :update, room), @invalid_params
      assert redirected_to(conn) == page_path(conn, :index)
    end

    should "DELETE /rooms/:id should not delete the record and should redirect to the home page", context do
      room = Repo.insert %Room{}
      conn = delete context.conn, room_path(context.conn, :delete, room)
      assert redirected_to(conn) == page_path(conn, :index)
      assert Repo.get(Room, room.id)
    end

  end
end
