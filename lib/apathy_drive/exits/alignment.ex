defmodule ApathyDrive.Exits.Alignment do
  use ApathyDrive.Exit

  def move(%Room{}, %Spirit{alignment: "evil"} = spirit, %{"min" => _min}) do
    Spirit.send_scroll(spirit, "<p>You are not evil enough to use this exit.</p>")
  end

  def move(%Room{}, %Spirit{alignment: "good"} = spirit, %{"max" => _max}) do
    Spirit.send_scroll(spirit, "<p>You are too evil to use this exit.</p>")
  end

  def move(%Room{} = room, %Spirit{} = spirit, room_exit),  do: super(room, spirit, room_exit)

  def move(%Room{} = room, %Monster{} = monster, %{"min" => _min} = room_exit) do
    if Monster.monster_alignment(monster) == "evil" do
      Monster.send_scroll(monster, "<p>You are not evil enough to use this exit.</p>")
    else
      super(room, monster, room_exit)
    end
  end

  def move(%Room{} = room, %Monster{} = monster, %{"max" => _max} = room_exit) do
    if Monster.monster_alignment(monster) == "good" do
      Monster.send_scroll(monster, "<p>You are too evil to use this exit.</p>")
    else
      super(room, monster, room_exit)
    end
  end

  def move(%Room{} = room, %Monster{} = monster, room_exit), do: super(room, monster, room_exit)
end
