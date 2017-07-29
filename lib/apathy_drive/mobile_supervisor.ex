defmodule ApathyDrive.MonsterSupervisor do
  import Supervisor.Spec
  require Ecto.Query
  alias ApathyDrive.{Repo, Monster}

  def start_link(children, opts) do
    Supervisor.start_link(children, Keyword.merge([strategy: :one_for_one], opts))
  end

  def launch(id) do
    id
    |> find_supervisor()
    |> Supervisor.start_child([id, [name: {:global, "monster_#{id}"}]])
  end

  def find_supervisor(id) do
    monster_template_id =
      Monster
      |> Ecto.Query.where([r], r.id == ^id)
      |> Ecto.Query.select([r], r.monster_template_id)
      |> Repo.one

    mt = "monster_template_#{monster_template_id}"

    children = [
      worker(ApathyDrive.Monster, [], restart: :transient)
    ]

    case Supervisor.start_child(__MODULE__, supervisor(__MODULE__, [children, [name: String.to_atom(mt), strategy: :simple_one_for_one]], [id: mt])) do
      {:ok, pid} ->
        pid
      {:error, {:already_started, pid}} ->
        pid
    end
  end
end