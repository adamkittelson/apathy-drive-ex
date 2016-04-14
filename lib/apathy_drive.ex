defmodule ApathyDrive do
  use Application

  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      worker(ApathyDrive.Endpoint, []),
      worker(ApathyDrive.Repo, []),
      worker(ApathyDrive.Migrator, [], restart: :temporary),
      supervisor(ApathyDrive.RoomSupervisor,   [[], [name: ApathyDrive.RoomSupervisor]]),
      supervisor(ApathyDrive.MobileSupervisor, [[], [name: ApathyDrive.MobileSupervisor]]),
      worker(ApathyDrive.Unity, [])
    ]

    # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: ApathyDrive.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    ApathyDrive.Endpoint.config_change(changed, removed)
    :ok
  end

end
