defmodule ApathyDrive.Player do
  use ApathyDrive.Web, :model
  alias ApathyDrive.Player
  import Comeonin.Bcrypt

  schema "players" do
    field :email,       :string
    field :password,    :string
    field :external_id, :string
    field :admin, :boolean, default: false

    has_many :characters, ApathyDrive.Character

    timestamps
  end

  def sign_up_changeset(model, params \\ :empty) do
    model
    |> cast(params, ~w(email password), [])
    |> validate_format(:email, ~r/@/)
    |> validate_length(:email, min: 3, max: 100)
    |> validate_length(:password, min: 6)
    |> validate_confirmation(:password)
  end

  def sign_in(email, password) do
    player = Repo.get_by(Player, email: email)
    sign_in?(player, password) && player
  end

  def sign_in?(%Player{password: stored_hash}, password) do
    checkpw(password, stored_hash)
  end

  def sign_in?(nil, _password) do
    dummy_checkpw
  end

  def find_or_create_by_external_id(external_id) do
    case Repo.one from s in Player, where: s.external_id == ^external_id do
      %Player{} = player ->
        player
      nil ->
        %Player{external_id: external_id}
        |> Repo.insert!
    end
  end
end
