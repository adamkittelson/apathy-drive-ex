defmodule ApathyDrive.Exits.Item do
  use ApathyDrive.Exit

  def move(%Room{} = room, %Spirit{} = spirit, room_exit),  do: super(room, spirit, room_exit)
  def move(%Room{} = room, %Monster{} = monster, %{"item" => item_template_id} = room_exit) do
    if item = get_item(monster, item_template_id) do
      #send(item.pid, :use)

      destination = Room.find(room_exit["destination"])
                    |> Room.value

      Room.send_scroll(destination, "<p><span class='dark-green'>#{interpolate(room_exit["to_message"], %{"user" => monster})}</span></p>")

      monster = monster
                |> Monster.set_room_id(room_exit["destination"])
                |> Monster.save

      Monster.send_scroll(monster, "<p><span class='yellow'>#{interpolate(room_exit["mover_message"], %{"user" => monster})}</span></p>")

      Room.look(destination, monster)

      Room.send_scroll(room, "<p><span class='dark-green'>#{interpolate(room_exit["from_message"], %{"user" => monster})}</span></p>")
      monster
    else
      Monster.send_scroll(monster, "<p><span class='yellow'>#{room_exit["failure_message"]}</span></p>")
    end

  end

  def get_item(%Monster{id: id}, item_template_id) do
    ApathyDrive.PubSub.subscribers("monsters:#{id}:items")
    |> Enum.map(&Item.value/1)
    |> Enum.find(fn(%Item{} = item) ->
         item.item_template_id == item_template_id
       end)
  end

end
