defmodule ApathyDrive.ItemTest do
  use ApathyDrive.ModelCase

  alias ApathyDrive.Item

  @valid_attrs %{description: "some content", grade: "light", level: 42,  name: "some content", worn_on: "some content"}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = Item.changeset(%Item{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = Item.changeset(%Item{}, @invalid_attrs)
    refute changeset.valid?
  end
end
