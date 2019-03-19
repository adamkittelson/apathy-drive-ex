defmodule ApathyDriveWeb.RaceControllerTest do
  use ApathyDriveWeb.ConnCase

  alias ApathyDrive.{Admin, Character}

  @create_attrs %{
    description: "some description",
    name: "some name",
    strength: 40,
    agility: 40,
    intellect: 40,
    willpower: 40,
    health: 40,
    charm: 40,
    stealth: false,
    exp_modifier: 15
  }
  @update_attrs %{description: "some updated description", name: "some updated name"}
  @invalid_attrs %{description: nil, name: nil}

  def fixture(:race) do
    {:ok, race} = Admin.create_race(@create_attrs)
    race
  end

  describe "index" do
    setup [:admin_user]

    test "lists all races", %{conn: conn} do
      conn = get(conn, Routes.race_path(conn, :index))
      assert html_response(conn, 200) =~ "Strength"
    end
  end

  describe "new race" do
    setup [:admin_user]

    test "renders form", %{conn: conn} do
      conn = get(conn, Routes.race_path(conn, :new))
      assert html_response(conn, 200) =~ "New Race"
    end
  end

  describe "create race" do
    setup [:admin_user]

    test "redirects to show when data is valid", %{conn: conn} do
      conn = post(conn, Routes.race_path(conn, :create), race: @create_attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == Routes.race_path(conn, :show, id)

      conn = get(conn, Routes.race_path(conn, :show, id))
      assert html_response(conn, 200) =~ "some name"
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.race_path(conn, :create), race: @invalid_attrs)
      assert html_response(conn, 200) =~ "New Race"
    end
  end

  describe "edit race" do
    setup [:create_race, :admin_user]

    test "renders form for editing chosen race", %{conn: conn, race: race} do
      conn = get(conn, Routes.race_path(conn, :edit, race))
      assert html_response(conn, 200) =~ "Edit Race"
    end
  end

  describe "update race" do
    setup [:create_race, :admin_user]

    test "redirects when data is valid", %{conn: conn, race: race} do
      conn = put(conn, Routes.race_path(conn, :update, race), race: @update_attrs)
      assert redirected_to(conn) == Routes.race_path(conn, :edit, race)

      conn = get(conn, Routes.race_path(conn, :show, race))
      assert html_response(conn, 200) =~ "some updated description"
    end

    test "renders errors when data is invalid", %{conn: conn, race: race} do
      conn = put(conn, Routes.race_path(conn, :update, race), race: @invalid_attrs)
      assert html_response(conn, 200) =~ "Edit Race"
    end
  end

  describe "delete race" do
    setup [:create_race, :admin_user]

    test "deletes chosen race", %{conn: conn, race: race} do
      conn = delete(conn, Routes.race_path(conn, :delete, race))
      assert redirected_to(conn) == Routes.race_path(conn, :index)

      assert_error_sent(404, fn ->
        get(conn, Routes.race_path(conn, :show, race))
      end)
    end
  end

  defp create_race(_) do
    race = fixture(:race)
    {:ok, race: race}
  end

  defp admin_user(context) do
    {:ok, character} =
      %Character{admin: true}
      |> Repo.insert()

    {:ok, put_in(context.conn, put_session(session_conn(), :character, character.id))}
  end
end
