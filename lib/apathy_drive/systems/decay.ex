defmodule Systems.Decay do
  use Systems.Reload
  import Timer, except: [start: 0]
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
      Components.Items.remove_item(room, corpse)
      Components.Items.get_items(corpse)
      |> Enum.each(fn(item) ->
           Components.Items.remove_item(corpse, item)
           Components.Items.add_item(room, item)
         end)
      Entities.save!(room)
      Entities.delete!(corpse)
    else
      Components.Decay.decay(corpse)
      Components.Decay.set_decay_at(corpse)
    end
  end

end
