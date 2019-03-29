defmodule ApathyDrive.DomainName do
  use GenServer
  require Logger

  def start_link do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def init(_) do
    state = %{
      client: client(),
      zone: Application.get_env(:dnsimple, :zone),
      account_id: Application.get_env(:dnsimple, :account_id)
    }

    Process.send_after(self(), :update_ip_address, :timer.seconds(60))

    {:ok, state}
  end

  def handle_info(:update_ip_address, state) do
    if Application.get_env(:dnsimple, :enabled, false) do
      update_ip_address(state)
      Process.send_after(self(), :update_ip_address, :timer.seconds(60))
      {:noreply, state}
    else
      Logger.info("Dnsimple disabled, exiting DomainName process.")
      {:stop, :normal, state}
    end
  end

  defp update_ip_address(state) do
    %{"id" => record_id, "content" => a_record_ip} = a_record(state)
    ip = ip_address()

    if a_record_ip != ip do
      body = %{
        "content" => ip,
        "ttl" => 60
      }

      Dnsimple.Client.patch(
        state.client,
        "/v2/#{state.account_id}/zones/#{state.zone}/records/#{record_id}",
        body
      )

      Logger.info("ip address for #{state.zone} updated from #{a_record_ip} to #{ip}")
    end
  end

  defp client do
    url = Application.get_env(:dnsimple, :base_url)
    token = Application.get_env(:dnsimple, :access_token)
    agent = ApathyDrive.Gossip.Core.user_agent()
    %Dnsimple.Client{base_url: url, access_token: token, user_agent: agent}
  end

  defp a_record(%{client: client} = state) do
    {:ok, %HTTPoison.Response{body: response}} =
      Dnsimple.Client.get(client, "/v2/#{state.account_id}/zones/#{state.zone}/records")

    response
    |> Jason.decode!()
    |> Map.get("data")
    |> Enum.find(&(&1["type"] == "A"))
  end

  defp ip_address do
    {:ok, 200, _headers, ip} = :hackney.get("https://api.ipify.org", [], [], with_body: true)
    ip
  end
end
