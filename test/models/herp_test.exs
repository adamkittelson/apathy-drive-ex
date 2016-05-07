defmodule ApathyDrive.HerpTest do
  use ApathyDrive.ModelCase

  alias ApathyDrive.Herp

  @valid_attrs %{derp: "some content"}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = Herp.changeset(%Herp{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = Herp.changeset(%Herp{}, @invalid_attrs)
    refute changeset.valid?
  end
end
