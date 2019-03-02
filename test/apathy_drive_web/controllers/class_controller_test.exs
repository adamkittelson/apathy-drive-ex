defmodule ApathyDriveWeb.ClassControllerTest do
  use ApathyDriveWeb.ConnCase

  alias ApathyDrive.{Admin, Character}

  @create_attrs %{
    description: "some description",
    name: "some name",
    weapon: "All",
    armour: "Platemail",
    stealth: false,
    exp_modifier: 100
  }
  @update_attrs %{description: "some updated description", name: "some updated name"}
  @invalid_attrs %{description: nil, name: nil}

  def fixture(:class) do
    {:ok, class} = Admin.create_class(@create_attrs)
    class
  end

  describe "index" do
    setup [:admin_user]

    test "lists all classes", %{conn: conn} do
      conn = get(conn, class_path(conn, :index))
      assert html_response(conn, 200) =~ "Description"
    end
  end

  describe "new class" do
    setup [:admin_user]

    test "renders form", %{conn: conn} do
      conn = get(conn, class_path(conn, :new))
      assert html_response(conn, 200) =~ "New Class"
    end
  end

  describe "create class" do
    setup [:admin_user]

    test "redirects to edit when data is valid", %{conn: conn} do
      conn = post(conn, class_path(conn, :create), class: @create_attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == class_path(conn, :edit, id)

      conn = get(conn, class_path(conn, :show, id))
      assert html_response(conn, 200) =~ "some name"
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, class_path(conn, :create), class: @invalid_attrs)
      assert html_response(conn, 200) =~ "New Class"
    end
  end

  describe "edit class" do
    setup [:create_class, :admin_user]

    test "renders form for editing chosen class", %{conn: conn, class: class} do
      conn = get(conn, class_path(conn, :edit, class))
      assert html_response(conn, 200) =~ "Edit Class"
    end
  end

  describe "update class" do
    setup [:create_class, :admin_user]

    test "redirects when data is valid", %{conn: conn, class: class} do
      conn = put(conn, class_path(conn, :update, class), class: @update_attrs)
      assert redirected_to(conn) == class_path(conn, :edit, class)

      conn = get(conn, class_path(conn, :show, class))
      assert html_response(conn, 200) =~ "some updated description"
    end

    test "renders errors when data is invalid", %{conn: conn, class: class} do
      conn = put(conn, class_path(conn, :update, class), class: @invalid_attrs)
      assert html_response(conn, 200) =~ "Edit Class"
    end
  end

  describe "delete class" do
    setup [:create_class, :admin_user]

    test "deletes chosen class", %{conn: conn, class: class} do
      conn = delete(conn, class_path(conn, :delete, class))
      assert redirected_to(conn) == class_path(conn, :index)

      assert_error_sent(404, fn ->
        get(conn, class_path(conn, :show, class))
      end)
    end
  end

  defp create_class(_) do
    class = fixture(:class)
    {:ok, class: class}
  end

  defp admin_user(context) do
    {:ok, character} =
      %Character{admin: true}
      |> Repo.insert()

    {:ok, put_in(context.conn, put_session(session_conn(), :character, character.id))}
  end
end
