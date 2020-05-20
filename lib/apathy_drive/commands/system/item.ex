defmodule ApathyDrive.Commands.System.Item do
  alias ApathyDrive.{
    Character,
    Item,
    Mobile,
    Repo,
    Room,
    ShopItem
  }

  def execute(%Room{} = room, character, ["create" | trait_name]) do
    trait_name = Enum.join(trait_name, " ")

    create(character, trait_name)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Item{}} = character, [
        "set",
        "description" | description
      ]) do
    set_description(character, description)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Item{}} = character, [
        "set",
        "weight" | description
      ]) do
    set_weight(character, description)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Item{}} = character, [
        "set",
        "type" | type
      ]) do
    set_type(character, type)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Item{}} = character, [
        "set",
        "getable" | getable
      ]) do
    set_getable(character, getable)

    room
  end

  def execute(%Room{} = room, %Character{editing: %Item{}} = character, [
        "set",
        "room_destruct_message" | room_destruct_message
      ]) do
    set_room_destruct_message(character, room_destruct_message)

    room
  end

  # def execute(%Room{} = room, %Character{editing: %Trait{}} = character, [
  #       "set",
  #       "name" | description
  #     ]) do
  #   set_name(character, description)

  #   room
  # end

  # def execute(%Room{} = room, %Character{editing: %Trait{}} = character, [
  #       "set",
  #       "merge_by" | description
  #     ]) do
  #   set_merge_by(character, description)

  #   room
  # end

  def execute(%Room{} = room, character, _args) do
    IO.inspect(character.editing)
    Mobile.send_scroll(character, "<p>Invalid system command.</p>")

    room
  end

  defp create(character, item_name) do
    item = Repo.insert!(%Item{name: item_name}, on_conflict: :nothing)

    Mobile.send_scroll(character, "<p>#{item.name} created.</p>")

    look(character, item)
  end

  defp set_description(character, description) do
    description = Enum.join(description, " ")
    item = character.editing

    item
    |> Item.set_description_changeset(description)
    |> Repo.update()
    |> case do
      {:ok, item} ->
        look(character, item)

      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect(changeset.errors)}</p>")
    end
  end

  defp set_room_destruct_message(character, room_destruct_message) do
    room_destruct_message = Enum.join(room_destruct_message, " ")
    item = character.editing

    item
    |> Item.set_room_destruct_message_changeset(room_destruct_message)
    |> Repo.update()
    |> case do
      {:ok, item} ->
        look(character, item)

      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect(changeset.errors)}</p>")
    end
  end

  defp set_weight(character, weight) do
    weight = Enum.join(weight, " ")
    item = character.editing

    item
    |> Item.set_weight_changeset(weight)
    |> Repo.update()
    |> case do
      {:ok, item} ->
        look(character, item)

      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect(changeset.errors)}</p>")
    end
  end

  defp set_type(character, type) do
    type = Enum.join(type, " ")
    item = character.editing

    item
    |> Item.set_type_changeset(type)
    |> Repo.update()
    |> case do
      {:ok, item} ->
        look(character, item)

      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect(changeset.errors)}</p>")
    end
  end

  defp set_getable(character, getable) do
    getable = Enum.join(getable, " ")
    item = character.editing

    item
    |> Item.set_getable_changeset(getable)
    |> Repo.update()
    |> case do
      {:ok, item} ->
        look(character, item)

      {:error, changeset} ->
        Mobile.send_scroll(character, "<p>#{inspect(changeset.errors)}</p>")
    end
  end

  defp look(character, %Item{} = item) do
    item =
      %ShopItem{item: item}
      |> Item.from_assoc()

    ApathyDrive.Commands.Look.look_at_item(character, item)
  end

  # defp set_name(character, description) do
  #   description = Enum.join(description, " ")
  #   trait = character.editing

  #   trait
  #   |> Trait.set_name_changeset(description)
  #   |> Repo.update()
  #   |> case do
  #     {:ok, trait} ->
  #       Mobile.send_scroll(character, "<p>#{inspect(trait)}</p>")

  #     {:error, changeset} ->
  #       Mobile.send_scroll(character, "<p>#{inspect(changeset.errors)}</p>")
  #   end
  # end

  # defp set_merge_by(character, merge_by) do
  #   merge_by = Enum.join(merge_by, " ")
  #   trait = character.editing

  #   trait
  #   |> Trait.set_merge_by_changeset(merge_by)
  #   |> Repo.update()
  #   |> case do
  #     {:ok, trait} ->
  #       Mobile.send_scroll(character, "<p>#{inspect(trait)}</p>")

  #     {:error, changeset} ->
  #       Mobile.send_scroll(character, "<p>#{inspect(changeset.errors)}</p>")
  #   end
  # end
end
