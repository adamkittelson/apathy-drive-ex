defmodule Systems.Hints do
  use Systems.Reload

  def initialize do
    :timer.apply_interval(60_000, Systems.Hints, :display_hint, [])
  end

  def display_hint do
    Components.all(Components.Hints) |> Enum.each(fn(entity) ->
      unless Systems.Idle.idle?(entity) do
        hint = Components.Hints.value(entity).active
               |> Map.values
               |> Enum.shuffle
               |> List.first
        if hint do
          Components.Player.send_message(entity, ["scroll", "<p><span class='yellow'>Hint:</span> #{hint}<p>"])
        end
      end
    end)
  end
end
