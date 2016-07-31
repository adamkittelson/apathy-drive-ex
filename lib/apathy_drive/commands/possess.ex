defmodule ApathyDrive.Commands.Possess do
  use ApathyDrive.Command

  def keywords, do: ["possess"]

  def execute(%Room{} = room, %Mobile{} = mobile, []) do
    Mobile.send_scroll(mobile, "<p>Possess what?</p>")
    room
  end

  def execute(%Room{} = room, %Mobile{monster_template_id: nil} = mobile, args) do
    case Room.find_mobile_in_room(room, mobile, Enum.join(args, " ")) do
      target when target == mobile ->
        Mobile.send_scroll(mobile, "<p>Possess yourself?.</p>")
        room
      %Mobile{spirit: %Spirit{}} = mobile ->
        Mobile.send_scroll(mobile, "<p>#{Mobile.look_name(mobile.name)} is already possessed by another spirit.</p>")
        room
      %Mobile{ref: ref} ->
        room =
          update_in(room.mobiles[ref], fn(target) ->
            target =
              target
              |> Map.put(:spirit, mobile.spirit)
              |> Map.put(:socket, mobile.socket)
              |> Mobile.set_abilities
              |> Mobile.set_max_mana
              |> Mobile.set_mana
              |> Mobile.set_max_hp
              |> Mobile.set_hp

            send(mobile.socket, {:update_ref, target.ref})

            Mobile.send_scroll(target, "<p>You possess #{Mobile.look_name(target)}.")
            Mobile.update_prompt(target)

            target
          end)

        put_in(room.mobiles, Map.delete(room.mobiles, mobile.ref))
        |> Room.update_essence_targets
      _ ->
        Mobile.send_scroll(mobile, "<p>You don't see that here.</p>")
        room
    end
  end

  def execute(%Room{} = room, %Mobile{monster_template_id: _, name: name} = mobile, _args) do
    Mobile.send_scroll(mobile, "<p>You are already possessing #{name}.</p>")
    room
  end

end
