defmodule Systems.Decay do
  use Systems.Reload
  import Utility
  use Timex

  def initialize do
    every 10, do: decay
  end

  def decay do
    Corpses.all |> Enum.each(fn(corpse) ->
      decay_at = Components.Decay.decay_at(corpse)
      if decay_at do
        decay_at = decay_at
                   |> Date.convert :secs

        if decay_at < Date.convert(Date.now, :secs) do
          decay(corpse)
        end
      else
        Components.Decay.set_decay_at(corpse)
      end
    end)
  end

  def decay(corpse) do
    if Components.Decay.state(corpse) == "decayed" do
      room = Components.CurrentRoom.get_current_room(corpse)
      Components.Items.remove_item(room, corpse)
      Entities.save!(room)
      Entities.delete!(corpse)
    else
      Components.Decay.decay(corpse)
      Components.Decay.set_decay_at(corpse)
    end
  end

end
