defmodule ApathyDrive.Exits.Trap do
  use ApathyDrive.Exit
  alias ApathyDrive.Ability

  def move(%Room{} = room, %Spirit{} = spirit, room_exit),  do: super(room, spirit, room_exit)
  def move(%Room{} = room, %Monster{} = monster, room_exit) do
    destination = Room.find(room_exit["destination"])
                  |> Room.value

    Room.send_scroll(destination, "<p>#{interpolate(room_exit["to_message"], %{"user" => monster, "alignment-color" => Monster.alignment_color(monster)})}</p>")

    monster = monster
              |> Monster.set_room_id(destination.id)
              |> Monster.save

    Monster.send_scroll(monster, "<p><span class='red'>#{interpolate(room_exit["mover_message"] |> to_string, %{"user" => monster})}</span></p>")

    Room.send_scroll(room, "<p>#{interpolate(room_exit["from_message"], %{"user" => monster, "alignment-color" => Monster.alignment_color(monster)})}</p>")

    send(self, {:apply_ability, ability(room_exit), monster})

    Room.look(destination, monster)

    monster
  end

  def ability(room_exit) do
    %Ability{properties: %{"kind" => "attack",
                           "flags" => [],
                           "instant_effects" => %{"damage" => room_exit["damage"]}, "damage_type" => "normal"}}
  end

end
