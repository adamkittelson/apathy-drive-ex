defmodule ApathyDrive.RoomSupervisor do
  use Supervisor
  require Ecto.Query
  alias ApathyDrive.{Repo, Room, RoomServer}

  def start_link(children, opts) do
    Supervisor.start_link(__MODULE__, children, opts)
  end

  def init(children) do
    supervise(children, strategy: :one_for_one)
  end

  def launch(id) do
    id
    |> find_supervisor()
    |> case do
      pid when is_pid(pid) ->
        Supervisor.start_child(
          pid,
          {"room_#{id}", {GenServer, :start_link, [RoomServer, id, [name: :"room_#{id}"]]},
           :transient, 5000, :worker, [RoomServer]}
        )

      _ ->
        false
    end
  end

  def find_supervisor(id) do
    area =
      Room
      |> Ecto.Query.where(id: ^id)
      |> Ecto.Query.join(:left, [r], a in assoc(r, :area))
      |> Ecto.Query.select([r, a], a.name)
      |> Repo.one()

    if area do
      case Supervisor.start_child(
             __MODULE__,
             supervisor(__MODULE__, [[], [name: String.to_atom(area)]], id: area)
           ) do
        {:ok, pid} ->
          pid

        {:error, {:already_started, pid}} ->
          pid
      end
    end
  end
end
