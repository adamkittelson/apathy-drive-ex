defmodule ApathyDrive.Repo do
  use Ecto.Repo, otp_app: :apathy_drive
  use Scrivener, page_size: 10
  alias ApathyDrive.Room

  def save(%Room{room_unity: room_unity} = room) do
    room
    |> Map.put(:room_unity, save!(room_unity))
    |> save!()
  end

  def save!(%{:__struct__ => module} = current_struct) do
    [pkey] = module.__schema__(:primary_key)
    model = struct(module, [{pkey, Map.get(current_struct, pkey)}, {:__meta__, current_struct.__meta__}])

    changes =
      current_struct
      |> Map.from_struct
      |> Map.take(module.__schema__(:fields))
      |> Map.drop([:inserted_at, :updated_at, pkey])

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

    current_struct
    |> Map.merge(model
                 |> Ecto.Changeset.change(changes)
                 |> insert_or_update!
                 |> Map.take([pkey, :__meta__]))
  end

end
