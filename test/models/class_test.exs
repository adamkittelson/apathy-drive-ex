defmodule ApathyDrive.ClassTest do
  use ApathyDrive.ModelCase

  alias ApathyDrive.Class

  @valid_attrs %{description: "some content", name: "some content", properties: "{}"}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = Class.changeset(%Class{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = Class.changeset(%Class{}, @invalid_attrs)
    refute changeset.valid?
  end
end
