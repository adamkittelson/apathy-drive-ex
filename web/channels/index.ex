defmodule ApathyDrive.Index do
  use Phoenix.Channel

  def join("index", %{}, socket) do
    {:ok, %{stats: ApathyDrive.Factions.war_status}, socket}
  end

end
