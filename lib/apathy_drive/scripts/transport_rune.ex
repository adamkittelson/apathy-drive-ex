defmodule ApathyDrive.Scripts.TransportRune do
  alias ApathyDrive.{Item, ItemInstance, Mobile, Repo, Room}

  @exploding_rune_item_id 11

  def execute(%Room{} = room, mobile_ref, _target_ref) do
    Room.update_mobile(room, mobile_ref, fn room, mobile ->
      case Mobile.ability_value(mobile, "Beacon") do
        0 ->
          Mobile.send_scroll(
            mobile,
            "<p><span class='dark-yellow'>Unable to locate your beacon, the rune fades away.</span></p>"
          )

          room

        room_id ->
          %ItemInstance{
            item_id: @exploding_rune_item_id,
            room_id: room.id,
            character_id: nil,
            owner_id: mobile.id,
            equipped: false,
            hidden: false,
            delete_at: Timex.shift(DateTime.utc_now(), minutes: 12),
            beacon_room_id: room_id
          }
          |> Repo.insert!()

          Room.load_items(room)
      end
    end)
  end

  def activate(room, mobile) do
    Enum.reduce(room.items, room, fn
      %Item{id: @exploding_rune_item_id, beacon_room_id: room_id}, room ->
        Room.update_mobile(room, mobile.ref, fn
          room, %{} = mobile ->
            room_exit = %{
              "kind" => "Action",
              "destination" => room_id,
              "mover_message" =>
                "<span class='blue'>You step on a transport rune and are teleported somewhere else!</span>",
              "from_message" =>
                "<span class='blue'>{{Name}} steps on a transport rune and vanishes!</span>",
              "to_message" => "<span class='blue'>{{Name}} appears out of thin air!</span>"
            }

            case ApathyDrive.Commands.Move.execute(room, mobile, room_exit, 0) do
              %Room{} = room ->
                room

              {:error, :too_tired, room} ->
                room
            end
        end)

      _item, room ->
        room
    end)
  end
end
