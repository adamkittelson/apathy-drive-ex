defmodule ApathyDrive.ClassTest do
  use ApathyDrive.ModelCase

  alias ApathyDrive.Class

  @valid_attrs %{"alignment" => "good", "name" => "Adam", "start_room_id" => 42, "unities" => ["good"]}
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
