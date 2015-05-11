defmodule ApathyDrive.Index do
  use Phoenix.Channel

  def join("index", %{}, socket) do
    send(self, :update_war_stats)

    {:ok, socket}
  end

  def handle_info(:update_war_stats, socket) do
    Phoenix.Channel.push socket, "war-status", %{stats: ApathyDrive.Factions.war_status}

    {:noreply, socket}
  end

end
