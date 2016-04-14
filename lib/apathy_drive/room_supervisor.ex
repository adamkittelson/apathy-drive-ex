defmodule ApathyDrive.RoomSupervisor do
  use Supervisor
  require Ecto.Query
  alias ApathyDrive.Repo

  def start_link(children, opts) do
    Supervisor.start_link(__MODULE__, children, opts)
  end

  def init(children) do
    supervise(children, [strategy: :one_for_one])
  end

  def launch(id) do
    id
    |> find_supervisor()
    |> Supervisor.start_child({"room_#{id}", {GenServer, :start_link, [Room, id, [name: {:global, "room_#{id}"}]]}, :permanent, 5000, :worker, [Room]})
  end

  def find_supervisor(id) do
    area =
      Room
      |> Ecto.Query.where([r], r.id == ^id)
      |> Ecto.Query.select([r], r.area)
      |> Repo.one

    case Supervisor.start_child(__MODULE__, supervisor(__MODULE__, [[], [name: String.to_atom(area)]], [id: area])) do
      {:ok, pid} ->
        pid
      {:error, {:already_started, pid}} ->
        pid
    end
  end
end