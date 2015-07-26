defmodule ApathyDrive.RaceTest do
  use ApathyDrive.ModelCase

  alias ApathyDrive.Race

  @valid_attrs %{name: "Dwarves", description: "short and stocky", properties: ~s({"encumbrance": 20})}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = Race.changeset(%Race{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = Race.changeset(%Race{}, @invalid_attrs)
    refute changeset.valid?
  end
end
