defmodule ApathyDrive.AdminTest do
  use ApathyDrive.DataCase

  alias ApathyDrive.Admin

  describe "classes" do
    alias ApathyDrive.Class

    @valid_attrs %{
      description: "some description",
      name: "some name",
      weapon: "All",
      armour: "Plate",
      stealth: false,
      exp_modifier: 100
    }
    @update_attrs %{description: "some updated description", name: "some updated name"}
    @invalid_attrs %{description: nil, name: nil}

    def class_fixture(attrs \\ %{}) do
      {:ok, class} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Admin.create_class()

      class
    end

    test "list_classes/0 returns all classes" do
      class = class_fixture()
      assert Admin.list_classes() == [class]
    end

    test "get_class!/1 returns the class with given id" do
      class =
        class_fixture()
        |> Map.put(:classes_traits, [])

      assert Admin.get_class!(class.id) == class
    end

    test "create_class/1 with valid data creates a class" do
      assert {:ok, %Class{} = class} = Admin.create_class(@valid_attrs)
      assert class.description == "some description"
      assert class.name == "some name"
    end

    test "create_class/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Admin.create_class(@invalid_attrs)
    end

    test "update_class/2 with valid data updates the class" do
      class = class_fixture()

      assert {:ok, class} = Admin.update_class(class, @update_attrs)
      assert %Class{} = class
      assert class.description == "some updated description"
      assert class.name == "some updated name"
    end

    test "update_class/2 with invalid data returns error changeset" do
      class =
        class_fixture()
        |> Map.put(:classes_traits, [])

      assert {:error, %Ecto.Changeset{}} = Admin.update_class(class, @invalid_attrs)
      assert class == Admin.get_class!(class.id)
    end

    test "delete_class/1 deletes the class" do
      class = class_fixture()
      assert {:ok, %Class{}} = Admin.delete_class(class)
      assert_raise Ecto.NoResultsError, fn -> Admin.get_class!(class.id) end
    end

    test "change_class/1 returns a class changeset" do
      class = class_fixture()
      assert %Ecto.Changeset{} = Admin.change_class(class)
    end
  end
end
