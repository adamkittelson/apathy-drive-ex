defmodule ApathyDrive.TimerTest do
  use ExUnit.Case, async: true

  alias ApathyDrive.{Character, Item, Room}

  describe "Room.next_timer" do
    setup [:room_with_character]

    test "Room.next_timer/1", %{room: room, mobile_ref: ref} do
      assert nil == Room.next_timer(room)

      room = ApathyDrive.Scripts.RaiseWeapon.execute(room, ref, nil)

      refute is_nil(Room.next_timer(room))

      room = Room.apply_timers(room)

      assert_receive :hello

      assert is_nil(Room.next_timer(room))
    end
  end

  defp room_with_character(context) do
    ref = make_ref()

    room = %Room{
      light: 0,
      mobiles: %{
        ref => %Character{
          ref: ref,
          socket: self,
          name: "Cole",
          race: %{race: %{name: "Elf", stealth: true}},
          class: %{class: %{name: "Ranger", stealth: true}},
          strength: 50,
          agility: 50,
          intellect: 50,
          willpower: 50,
          health: 50,
          charm: 50,
          level: 1,
          equipment: [
            %Item{
              worn_on: "Held",
              instance_id: 123
            }
          ]
        }
      }
    }

    context
    |> Map.put(:room, room)
    |> Map.put(:mobile_ref, ref)
  end
end
