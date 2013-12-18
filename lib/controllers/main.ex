defmodule ApathyDrive.Main do

  use Weber.Controller

  layout false
  def action([], _conn) do
    {:render, [], []}
  end

end
