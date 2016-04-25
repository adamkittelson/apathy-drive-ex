defmodule ApathyDrive.Commands.Unpossess do
  use ApathyDrive.Command

  def keywords, do: ["unpossess"]

  def execute(mobile, _arguments) when is_pid(mobile) do
    Mobile.unpossess(mobile)
  end


  def execute(%Mobile{monster_template_id: nil} = mobile) do
    Mobile.send_scroll(mobile, "<p>You aren't possessing anything.</p>")
  end
  def execute(%Mobile{socket: socket, spirit: spirit} = mobile) do
    mobile =
      mobile
      |> Map.put(:spirit, nil)
      |> Map.put(:socket, nil)
      |> Mobile.set_abilities
      |> Mobile.set_max_mana
      |> Mobile.set_mana
      |> Mobile.set_max_hp
      |> Mobile.set_hp

    Process.unregister(:"spirit_#{spirit.id}")

    ApathyDrive.PubSub.unsubscribe("spirits:online")
    ApathyDrive.PubSub.unsubscribe("spirits:#{spirit.id}")
    ApathyDrive.PubSub.unsubscribe("chat:gossip")
    ApathyDrive.PubSub.unsubscribe("chat:#{String.downcase(spirit.class.name)}")

    send(socket, {:respawn, spirit: spirit})
    send(socket, {:scroll, "<p>You leave the body of #{Mobile.look_name(mobile)}.</p>"})

    mobile
  end

end
