defmodule Systems.Decay do

  import BlockTimer
  use Timex

  def initialize do
    apply_interval 10 |> seconds, do: decay
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
      room = Parent.of(corpse)
      if room do
        Components.Items.remove_item(room, corpse)
        Entities.save!(room)
      end
      Entities.delete!(corpse)
    else
      Components.Decay.decay(corpse)
      Components.Decay.set_decay_at(corpse)
    end
  end

end
