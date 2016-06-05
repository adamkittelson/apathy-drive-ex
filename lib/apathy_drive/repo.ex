defmodule ApathyDrive.Repo do
  use Ecto.Repo, otp_app: :apathy_drive
  use Scrivener, page_size: 3
  alias ApathyDrive.Room

  def save(%Room{room_unity: room_unity} = room) do
    room
    |> Map.put(:room_unity, save!(room_unity))
    |> save!()
  end

  def save!(struct) do
    module    = struct.__struct__
    model     = struct(struct.__struct__, [id: struct.id, __meta__: struct.__meta__])
    changes   = struct |> Map.from_struct
                       |> Map.take(module.__schema__(:fields))
                       |> Map.drop([:inserted_at, :updated_at, :id])


    model =
      changes
      |> Map.keys
      |> Enum.reduce(model, fn(field, model) ->
           if Map.get(model, field) == Map.get(changes, field) do
             Map.put(model, field, "_change_me")
           else
             model
           end
         end)

    struct
    |> Map.merge(model
                 |> Ecto.Changeset.change(changes)
                 |> insert_or_update!
                 |> Map.take([:id, :__meta__]))
  end

end
