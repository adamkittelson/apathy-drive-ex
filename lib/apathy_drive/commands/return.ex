defmodule ApathyDrive.Commands.Return do
  use ApathyDrive.Command
  alias ApathyDrive.Mobile

  def keywords, do: ["return"]

  def execute(mobile, _args) when is_pid(mobile) do
    Mobile.return(mobile)
  end

  def execute(%Mobile{} = mobile) do
    import Mobile

    if !held(mobile) do
      mobile.room_id
      |> Room.find
      |> Room.display_exit_message(%{name: look_name(mobile), mobile: self, message: "<span class='blue'>{{Name}} vanishes into thin air!</span>", to: nil})

      ApathyDrive.PubSub.unsubscribe("rooms:#{mobile.room_id}:mobiles")
      ApathyDrive.PubSub.unsubscribe("rooms:#{mobile.room_id}:mobiles:#{mobile.alignment}")

      destination_id = mobile.spirit.class.start_room_id
      mobile =
        mobile
        |> Map.put(:room_id, destination_id)
        |> Map.put(:last_room, nil)

      ApathyDrive.PubSub.subscribe("rooms:#{destination_id}:mobiles")
      ApathyDrive.PubSub.subscribe("rooms:#{destination_id}:mobiles:#{mobile.alignment}")

      ApathyDrive.PubSub.unsubscribe("rooms:#{mobile.spirit.room_id}:spirits")
      mobile = put_in(mobile.spirit.room_id, mobile.room_id)
      ApathyDrive.PubSub.subscribe("rooms:#{mobile.spirit.room_id}:spirits")

      destination = Room.find(destination_id)

      Room.audible_movement({:global, "room_#{destination_id}"}, nil)

      Mobile.send_scroll(mobile, "<p><span class='blue'>You teleport home.</span></p>")

      Mobile.look(self)

      Room.display_enter_message(destination, %{name: look_name(mobile), mobile: self, message: "<span class='blue'>{{Name}} appears out of thin air!</span>", from: nil})

      notify_presence(mobile)

      mobile

    else
      mobile
    end
  end

end
