defmodule ApathyDrive.RoomSupervisor do
  use Supervisor
  require Ecto.Query
  alias ApathyDrive.{Repo, Room, RoomServer}

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, [], opts)
  end

  def start_link(children, opts) do
    Supervisor.start_link(__MODULE__, children, opts)
  end

  def init(children) do
    Supervisor.init(children, strategy: :one_for_one)
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

  def find_supervisor(nil), do: nil

  def find_supervisor(id) do
    area =
      Room
      |> Ecto.Query.where(id: ^id)
      |> Ecto.Query.join(:left, [r], a in assoc(r, :area))
      |> Ecto.Query.select([r, a], a.name)
      |> Repo.one()

    area = area || "unassigned"

    if area do
      case Supervisor.start_child(__MODULE__, area_child_spec(area)) do
        {:ok, pid} ->
          pid

        {:error, {:already_started, pid}} ->
          pid
      end
    end
  end

  def area_child_spec(area) do
    %{
      id: area,
      start: {__MODULE__, :start_link, [[], [name: String.to_atom(area)]]}
    }
  end
end
