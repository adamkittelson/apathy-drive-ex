defmodule Hint do
  use ApathyDrive.Web, :model

  alias ApathyDrive.Repo

  schema "hints" do
    field :name,       :string
    field :body,       :string

    timestamps()
  end

  def create(name, body) do
    %Hint{name: name, body: body}
    |> Repo.insert!
  end

  def find_by_name(name) do
    query = from h in Hint,
            where: h.name == ^name,
            select: h.body

    Repo.one(query)
  end

  def random([]),         do: nil
  def random(hint_names) do
    hint_names
    |> Enum.random
    |> find_by_name
  end

end