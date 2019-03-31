defmodule ApathyDrive.RoomExit do
  use ApathyDriveWeb, :model
  alias ApathyDrive.{Class, Room, RoomExit, Exit, Item}

  schema "rooms_exits" do
    field(:direction, :string)
    field(:data, ApathyDrive.JSONB)

    field(:delete, :boolean, virtual: true)

    belongs_to(:exit, Exit)
    belongs_to(:class, Class)
    belongs_to(:room, Room)
    belongs_to(:item, Item)
    belongs_to(:destination, Room)
  end

  @required_fields ~w(direction data)a

  def load_exits(room_id) do
    __MODULE__
    |> where([re], re.room_id == ^room_id)
    |> preload([:exit])
    |> preload([:destination])
    |> Repo.all()
    |> Enum.map(fn room_exit ->
      exit_data =
        %{
          "direction" => room_exit.direction,
          "area" => room_exit.destination.area_id,
          "zone" => room_exit.destination.zone_controller_id,
          "kind" => room_exit.exit.kind,
          "destination" => room_exit.destination.id
        }
        |> Map.merge(room_exit.data)

      cond do
        exit_data["kind"] == "Item" ->
          Map.put(exit_data, "item", room_exit.item_id)

        exit_data["kind"] == "Key" ->
          Map.put(exit_data, "key", room_exit.item_id)

        exit_data["kind"] == "Class" ->
          Map.put(exit_data, "class_id", room_exit.class_id)

        :else ->
          exit_data
      end
    end)
  end

  def changeset(%RoomExit{} = rt, attrs) do
    rt
    |> cast(attrs, [:delete | @required_fields])
    |> validate_required(@required_fields)
    |> mark_for_deletion()
  end

  defp mark_for_deletion(changeset) do
    # If delete was set and it is true, let's change the action
    if get_change(changeset, :delete) do
      %{changeset | action: :delete}
    else
      changeset
    end
  end
end
