defmodule ApathyDrive.ExAdmin.Room do
  use ExAdmin.Register

  register_resource ApathyDrive.Room do

    index do
      selectable_column()

      column :legacy_id
      column :name
      column :lair_size

      actions()
    end

    show room do
      attributes_table do
        row :legacy_id
        row :name
        row :lair_size
      end

      panel "Shop" do
        table_for room.items_for_sales do
          column :id, link: true
          column :name, link: true
        end
      end

      panel "Lair" do
        table_for room.lair_monsters do
          column :id, link: true
          column :name, link: true
        end
      end
    end


    # controller do
    #   before_filter :before_delete, only: [:destroy]
    #   after_filter :after_update, only: [:update]
    #
    #   def before_delete(%Plug.Conn{assigns: %{resource: %Room{id: id}}} = conn, _params) do
    #
    #     :global.send("room_#{id}", :room_deleted)
    #
    #     conn
    #   end
    #
    #   def after_update(conn, _params, %Room{id: id} = room, :update) do
    #     changes =
    #       room
    #       |> Map.from_struct
    #       |> Map.take(Room.__schema__(:fields))
    #       |> Map.drop([:inserted_at, :updated_at, :id])
    #
    #     ApathyDrive.PubSub.broadcast!("rooms:#{id}", {:room_updated, changes})
    #
    #     directions =
    #       changes.exits
    #       |> Enum.filter(&(&1["kind"] in ["Normal", "Action", "Door", "Gate", "Trap", "Cast"]))
    #       |> Enum.map(&(&1["direction"]))
    #
    #     ApathyDrive.Endpoint.broadcast!("map", "room admin updated", %{id: id, name: changes.name, coords: changes.coordinates, directions: directions})
    #
    #     conn
    #   end
    # end
    #
    form room do
      inputs do

        input room, :name
        input room, :description
        input room, :light
        content do
          ~s(
          <div id="room_item_descriptions_input" class="form-group">
            <label class="col-sm-2 control-label" for="room_item_descriptions">
              Item Descriptions<abbr class="required" title="required">*</abbr>
            </label>
            <div class="col-sm-10">
              <textarea id="room_item_descriptions" name="room[item_descriptions]" class="json">#{Poison.encode!(room.item_descriptions)}</textarea>
            </div>
          </div>
          )
        end
        input room, :lair_size
        input room, :lair_frequency
        content do
          ~s(
          <div id="room_exits_input" class="form-group">
            <label class="col-sm-2 control-label" for="room_exits">
              Exits<abbr class="required" title="required">*</abbr>
            </label>
            <div class="col-sm-10">
              <textarea id="room_exits" name="room[exits]" class="json">#{Poison.encode!(room.exits)}</textarea>
            </div>
          </div>
          )
        end
        content do
          ~s(
          <div id="room_commands_input" class="form-group">
            <label class="col-sm-2 control-label" for="room_commands">
              Commands<abbr class="required" title="required">*</abbr>
            </label>
            <div class="col-sm-10">
              <textarea id="room_commands" name="room[commands]" class="json">#{Poison.encode!(room.commands)}</textarea>
            </div>
          </div>
          )
        end
        input room, :legacy_id
        content do
          ~s(
          <div id="room_coordinates_input" class="form-group">
            <label class="col-sm-2 control-label" for="room_coordinates">
              Coordinates<abbr class="required" title="required">*</abbr>
            </label>
            <div class="col-sm-10">
              <textarea id="room_coordinates" name="room[coordinates]" class="json">#{Poison.encode!(room.coordinates)}</textarea>
            </div>
          </div>
          )
        end

      end

      inputs "Shop Items" do
        inputs :items_for_sales, collection: ApathyDrive.Repo.all(ApathyDrive.Item), as: :check_boxes#, fields: [:id, :name]
      end

      inputs "Lair Monsters" do
        inputs :lair_monsters, collection: ApathyDrive.Repo.all(ApathyDrive.Monster) |> Enum.sort_by(& &1.name), as: :check_boxes#, fields: [:id, :name]
      end

    end

    query do
      %{
        all: [preload: [:items_for_sales, :lair_monsters]]
      }
    end
  end
end
