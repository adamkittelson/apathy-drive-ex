defmodule ApathyDrive.PlayerTest do
  use ApathyDrive.ModelCase

  alias ApathyDrive.Player

  @valid_attrs %{admin: true, external_id: "some content"}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = Player.changeset(%Player{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = Player.changeset(%Player{}, @invalid_attrs)
    refute changeset.valid?
  end
end
