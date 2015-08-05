defmodule ApathyDrive.Index do
  use ApathyDrive.Web, :channel

  def join("index", %{}, socket) do
    {:ok, %{stats: ApathyDrive.Factions.war_status}, socket}
  end

end
