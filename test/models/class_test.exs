defmodule ApathyDrive.ClassTest do
  use ApathyDrive.ModelCase

  alias ApathyDrive.Class

  @valid_attrs %{abilities: "some content", agility: 42, agility_per_level: 42, alignment: "some content", name: "some content", strength: 42, strength_per_level: 42, will: 42, will_per_level: 42}
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
