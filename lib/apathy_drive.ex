defmodule ApathyDrive do
  use Application

  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      worker(ApathyDriveWeb.Endpoint, []),
      worker(ApathyDrive.Repo, []),
      worker(ApathyDrive.Migrator, [], restart: :temporary),
      worker(ApathyDrive.Directory, []),
      supervisor(ApathyDrive.RoomSupervisor, [[], [name: ApathyDrive.RoomSupervisor]]),
      worker(ApathyDrive.Metrics, []),
      worker(ApathyDrive.WorldMap, []),
      worker(ApathyDrive, [], function: :load_shops, restart: :transient)
    ]

    # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: ApathyDrive.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    ApathyDriveWeb.Endpoint.config_change(changed, removed)
    :ok
  end

  def load_shops do
    Task.start_link(fn ->
      ApathyDrive.Shop.room_ids()
      |> Enum.each(&ApathyDrive.RoomServer.load/1)
    end)
  end
end
