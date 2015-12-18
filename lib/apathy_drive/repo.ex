defmodule ApathyDrive.Repo do
  use Ecto.Repo, otp_app: :apathy_drive
  use Scrivener, page_size: 3

  def save!(struct) do
    module    = struct.__struct__
    model     = struct(struct.__struct__, [id: struct.id,
                                           __meta__: struct.__meta__])
    changes =
      struct
      |> Map.from_struct()
      |> Map.take(module.__schema__(:fields))
      |> Map.delete(:updated_at)

    changeset = Ecto.Changeset.change(model, changes)

    resp = insert_or_update!(changeset)

    struct
    |> Map.put(:id, resp.id)
    |> Map.put(:__meta__, resp.__meta__)
  end

end
