defmodule ApathyDrive.Commands.Possess do
  use ApathyDrive.Command
  alias ApathyDrive.Repo

  def keywords, do: ["possess"]

  def execute(%Room{} = room, query, spirit_id, class_name, socket, possessor) do
    if target = Room.find_mobile_in_room(room, possessor, query) do
      Mobile.become_possessed(target.pid, spirit_id, class_name, socket, possessor)
    else
      Mobile.send_scroll(possessor, "<p>Possess what?</p>")
    end
  end

  def execute(%Mobile{monster_template_id: nil, room_id: room_id} = mobile, query) do
    room_id
    |> Room.find
    |> Room.possess(query, mobile.spirit.id, mobile.spirit.class.name, mobile.socket, self())
  end

  def execute(%Mobile{monster_template_id: _, name: name} = mobile, _args) do
    Mobile.send_scroll(mobile, "<p>You are already possessing #{name}.</p>")
  end

  def execute(mobile, []) do
    Mobile.send_scroll(mobile, "<p>Possess what?</p>")
  end

  def execute(mobile, arguments) do
    Mobile.possess(mobile, Enum.join(arguments, " "))
  end

  def become_possessed(%Mobile{monster_template_id: nil} = mobile, _spirit_id, _class, _socket, possessor) when possessor == self() do
    Mobile.send_scroll(possessor, "<p>Possess yourself?.</p>")
    mobile
  end

  def become_possessed(%Mobile{monster_template_id: nil} = mobile, _spirit_id, _class, _socket, possessor) do
    Mobile.send_scroll(possessor, "<p>You can't possess other players.</p>")
    mobile
  end

  def become_possessed(%Mobile{unity: unity} = mobile, _spirit_id, "Angel", _socket, possessor) when unity != "angel" do
    Mobile.send_scroll(possessor, "<p>You may only possess monsters who were spawned in a purified room.</p>")
    mobile
  end

  def become_possessed(%Mobile{spirit: nil, level: level} = mobile, spirit_id, _class, socket, possessor) do
    spirit =
      Repo.get!(Spirit, spirit_id)
      |> Repo.preload(:class)

    if spirit.level >= level do
      ApathyDrive.PubSub.subscribe(self, "spirits:online")
      ApathyDrive.PubSub.subscribe(self, "spirits:#{spirit.id}")
      ApathyDrive.PubSub.subscribe(self, "chat:gossip")
      ApathyDrive.PubSub.subscribe(self, "chat:#{String.downcase(spirit.class.name)}")
      ApathyDrive.PubSub.unsubscribe(self, "rooms:#{mobile.spawned_at}:spawned_monsters")

      mobile =
        mobile
        |> Map.put(:spirit, spirit)
        |> Map.put(:socket, socket)
        |> Mobile.set_abilities
        |> Mobile.set_max_mana
        |> Mobile.set_mana
        |> Mobile.set_max_hp
        |> Mobile.set_hp

      send(socket, {:update_mobile, self})

      Mobile.send_scroll(mobile, "<p>You possess #{mobile.name}.")

      Process.monitor(socket)
      Process.unregister(:"spirit_#{spirit.id}")
      Process.register(self, :"spirit_#{spirit.id}")

      Mobile.update_prompt(mobile)

      Mobile.possession_successful(possessor)

      mobile
    else
      Mobile.send_scroll(possessor, "<p>You are too low level to possess #{mobile.name}.</p>")
      mobile
    end
  end

  def become_possessed(mobile, _spirit_id, _class, _socket, possessor) do
    Mobile.send_scroll(possessor, "<p>#{mobile.name} is possessed by another player.</p>")
    mobile
  end

end
