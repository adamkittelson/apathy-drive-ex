defmodule ApathyDrive.Commands.Possess do
  use ApathyDrive.Command
  alias ApathyDrive.Class

  def keywords, do: ["possess"]

  def execute(%Room{} = room, %Mobile{} = mobile, []) do
    Mobile.send_scroll(mobile, "<p>Possess what?</p>")
    room
  end

  def execute(%Room{} = room, %Mobile{monster_template_id: nil, spirit: %Spirit{level: level, class: %Class{unities: unities}}} = mobile, args) do
    case Room.find_mobile_in_room(room, mobile, Enum.join(args, " ")) do
      target when target == mobile ->
        Mobile.send_scroll(mobile, "<p>Possess yourself?.</p>")
        room
      %Mobile{spirit: %Spirit{}} = target ->
        Mobile.send_scroll(mobile, "<p>#{Mobile.look_name(target)} is already possessed by another spirit.</p>")
        room
      %Mobile{unities: [], level: target_level} = target when target_level > level ->
        Mobile.send_scroll(mobile, "<p>Your level is too low to possess #{Mobile.look_name(target)}.</p>")
        room
      %Mobile{ref: ref, unities: target_unities} ->
        if Enum.all?(target_unities, &(&1 in unities)) do
          possess(room, mobile, ref)
        else
          Mobile.send_scroll(mobile, "<p>You cannot possess monsters under the influence of your enemies.</p>")
          room
        end
      %Mobile{ref: ref} ->
        possess(room, mobile, ref)
      _ ->
        Mobile.send_scroll(mobile, "<p>You don't see that here.</p>")
        room
    end
  end

  def execute(%Room{} = room, %Mobile{monster_template_id: _, name: name} = mobile, _args) do
    Mobile.send_scroll(mobile, "<p>You are already possessing #{name}.</p>")
    room
  end

  def possess(%Room{} = room, %Mobile{} = mobile, ref) do
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
  end

end
