defmodule ApathyDrive.CharacterTest do
  use ApathyDrive.ModelCase

  alias ApathyDrive.Character

  @valid_attrs %{alignment: 42, class_id: 42, experience: 42, name: "some content", race_id: 42}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = Character.changeset(%Character{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = Character.changeset(%Character{}, @invalid_attrs)
    refute changeset.valid?
  end
end
