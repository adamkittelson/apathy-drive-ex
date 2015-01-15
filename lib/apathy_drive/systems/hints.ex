defmodule Systems.Hints do
  use Systems.Reload
  import Utility
  import BlockTimer

  def start_link do
    Task.start_link fn ->
      display_hints
    end
  end

  def display_hints do
    Spirits.all
    |> Enum.reject(&Systems.Idle.idle?/1)
    |> Enum.each(fn(spirit) ->

         hint = spirit
                |> Spirit.hints
                |> Hint.random

         if hint do
           send_message(spirit, "scroll", "<p>\n<span class='yellow'>Hint:</span> <em>#{hint}</em>\n\n<p>")
         end
       end)
    :timer.sleep 60000
  end
end
