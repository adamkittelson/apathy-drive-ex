defmodule ApathyDrive.Admin do
  @moduledoc """
  The Admin context.
  """

  import Ecto.Query, warn: false
  alias ApathyDrive.Repo

  alias ApathyDrive.{Ability, Class, ClassAbility, Race}

  @doc """
  Returns the list of classes.

  ## Examples

      iex> list_classes()
      [%Class{}, ...]

  """
  def list_classes do
    Class
    |> Ecto.Query.order_by(asc: :id)
    |> Repo.all()
  end

  def list_races do
    Race
    |> Ecto.Query.order_by(asc: :id)
    |> Repo.all()
  end

  def list_abilities do
    Ability
    |> Ecto.Query.order_by(asc: :id)
    |> Repo.all()
  end

  def get_abilities_for_class(%Class{id: id}) do
    ClassAbility
    |> where([mt], mt.class_id == ^id)
    |> preload([:ability])
    |> Ecto.Query.order_by(asc: :level)
    |> Repo.all()
  end

  @doc """
  Gets a single class.

  Raises `Ecto.NoResultsError` if the Class does not exist.

  ## Examples

      iex> get_class!(123)
      %Class{}

      iex> get_class!(456)
      ** (Ecto.NoResultsError)

  """
  def get_class!(id), do: Repo.get!(Class, id)

  def get_race!(id), do: Repo.get!(Race, id)

  @doc """
  Creates a class.

  ## Examples

      iex> create_class(%{field: value})
      {:ok, %Class{}}

      iex> create_class(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_class(attrs \\ %{}) do
    %Class{}
    |> Class.changeset(attrs)
    |> Repo.insert()
  end

  def create_race(attrs \\ %{}) do
    %Race{}
    |> Race.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a class.

  ## Examples

      iex> update_class(class, %{field: new_value})
      {:ok, %Class{}}

      iex> update_class(class, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_class(%Class{} = class, attrs) do
    class
    |> Class.changeset(attrs)
    |> Repo.update()
  end

  def update_race(%Race{} = race, attrs) do
    race
    |> Race.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a Class.

  ## Examples

      iex> delete_class(class)
      {:ok, %Class{}}

      iex> delete_class(class)
      {:error, %Ecto.Changeset{}}

  """
  def delete_class(%Class{} = class) do
    Repo.delete(class)
  end

  def delete_race(%Race{} = race) do
    Repo.delete(race)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking class changes.

  ## Examples

      iex> change_class(class)
      %Ecto.Changeset{source: %Class{}}

  """
  def change_class(%Class{} = class) do
    Class.changeset(class, %{})
  end

  def change_race(%Race{} = race) do
    Race.changeset(race, %{})
  end
end
