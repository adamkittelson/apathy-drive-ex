defmodule ApathyDrive.MobileSupervisor do
  use Supervisor
  require Ecto.Query
  alias ApathyDrive.{Repo, Mobile}

  def start_link(children, opts) do
    Supervisor.start_link(__MODULE__, children, opts)
  end

  def init(children) do
    supervise(children, [strategy: :one_for_one])
  end

  def launch(id) do
    id
    |> find_supervisor()
    |> Supervisor.start_child({"mobile_#{id}", {GenServer, :start_link, [Mobile, id, [name: {:global, "mobile_#{id}"}]]}, :transient, 5000, :worker, [Mobile]})
  end

  def find_supervisor(id) do
    monster_template_id =
      Mobile
      |> Ecto.Query.where([r], r.id == ^id)
      |> Ecto.Query.select([r], r.monster_template_id)
      |> Repo.one

    mt = "monster_template_#{monster_template_id}"

    case Supervisor.start_child(__MODULE__, supervisor(__MODULE__, [[], [name: String.to_atom(mt)]], [id: mt])) do
      {:ok, pid} ->
        pid
      {:error, {:already_started, pid}} ->
        pid
    end
  end
end