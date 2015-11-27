defmodule ApathyDrive.ItemTest do
  use ApathyDrive.ModelCase

  alias ApathyDrive.Item

  @valid_attrs %{agility: 42, description: "some content", grade: 42, level: 42, magical_defense: 42, name: "some content", physical_defense: 42, strength: 42, weight: 42, will: 42, worn_on: "some content"}
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
