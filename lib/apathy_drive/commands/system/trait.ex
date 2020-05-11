defmodule ApathyDrive.Commands.System.Trait do
  alias ApathyDrive.{
    Character,
    Mobile,
    Repo,
    Room,
    Trait
  }

  def execute(%Room{} = room, character, ["create" | trait_name]) do
    trait_name = Enum.join(trait_name, " ")

    create(character, trait_name)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Trait{}} = character, [
        "set",
        "description" | description
      ]) do
    set_description(character, description)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Trait{}} = character, [
        "set",
        "name" | description
      ]) do
    set_name(character, description)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Trait{}} = character, [
        "set",
        "merge_by" | description
      ]) do
    set_merge_by(character, description)

    room
  end

  def execute(%Room{} = room, character, _args) do
    Mobile.send_scroll(character, "<p>Invalid system command.</p>")

    room
  end

  defp create(character, trait_name) do
    trait = Repo.insert!(%Trait{name: trait_name}, on_conflict: :nothing)

    Mobile.send_scroll(character, "<p>#{inspect(trait)}</p>")
  end

  defp set_description(character, description) do
    description = Enum.join(description, " ")
    trait = character.editing

    trait
    |> Trait.set_description_changeset(description)
    |> Repo.update()
    |> case do
      {:ok, trait} ->
        Mobile.send_scroll(character, "<p>#{inspect(trait)}</p>")

      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect(changeset.errors)}</p>")
    end
  end

  defp set_name(character, description) do
    description = Enum.join(description, " ")
    trait = character.editing

    trait
    |> Trait.set_name_changeset(description)
    |> Repo.update()
    |> case do
      {:ok, trait} ->
        Mobile.send_scroll(character, "<p>#{inspect(trait)}</p>")

      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect(changeset.errors)}</p>")
    end
  end

  defp set_merge_by(character, merge_by) do
    merge_by = Enum.join(merge_by, " ")
    trait = character.editing

    trait
    |> Trait.set_merge_by_changeset(merge_by)
    |> Repo.update()
    |> case do
      {:ok, trait} ->
        Mobile.send_scroll(character, "<p>#{inspect(trait)}</p>")

      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect(changeset.errors)}</p>")
    end
  end
end
