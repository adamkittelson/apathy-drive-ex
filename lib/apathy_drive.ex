defmodule ApathyDrive do
  use Application

  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    :ok = ApathyDrive.Statix.connect()

    children = [
      {Phoenix.PubSub, name: ApathyDrive.PubSub},
      {ApathyDriveWeb.Endpoint, []},
      {ApathyDrive.Repo, []},
      {ApathyDrive.Migrator, []},
      {ApathyDrive.Trait, []},
      {ApathyDrive.Directory, []},
      {ApathyDrive.RoomSupervisor, [name: ApathyDrive.RoomSupervisor]},
      {ApathyDrive.Metrics, []},
      {ApathyDrive.WorldMap, []},
      {ApathyDrive, []},
      {ApathyDrive.DomainName, []}
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

  def child_spec(_arg) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :load_shops, []},
      restart: :transient
    }
  end

  def load_shops do
    import Ecto.Query

    Task.start_link(fn ->
      query =
        from(
          monster in ApathyDrive.Monster,
          where: not is_nil(monster.regen_time_in_hours) and monster.game_limit == 1,
          select: monster.id
        )

      boss_ids =
        query
        |> ApathyDrive.Repo.all()

      query =
        from(
          room in ApathyDrive.Room,
          where: room.permanent_npc in ^boss_ids,
          select: room.id
        )

      perm_ids =
        query
        |> ApathyDrive.Repo.all()
        |> Enum.uniq()

      query =
        from(
          lair in ApathyDrive.LairMonster,
          where: lair.monster_id in ^boss_ids,
          select: lair.room_id
        )

      lair_ids =
        query
        |> ApathyDrive.Repo.all()
        |> Enum.uniq()

      shop_room_ids = ApathyDrive.Shop.room_ids()

      room_ids = Enum.uniq(perm_ids ++ lair_ids ++ shop_room_ids)

      room_ids
      |> Enum.each(fn room_id ->
        Process.sleep(100)

        Task.start(fn ->
          ApathyDrive.RoomServer.load(room_id)
        end)
      end)
    end)
  end
end
