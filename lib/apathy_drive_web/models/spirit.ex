defmodule Spirit do
  use Ecto.Schema
  use ApathyDrive.Web, :model

  require Logger
  import Comeonin.Bcrypt
  alias ApathyDrive.{Mobile, PubSub, Room}

  schema "spirits" do
    belongs_to :room, Room
    belongs_to :class, ApathyDrive.Class
    has_many :recipes, ApathyDrive.SpiritItemRecipe
    has_many :recipe_items, through: [:recipes, :item]

    field :name,              :string
    field :alignment,         :string, virtual: true
    field :email,             :string
    field :password,          :string
    field :external_id,       :string
    field :experience,        :integer, default: 0
    field :level,             :integer, default: 1
    field :pid,               :any, virtual: true
    field :idle,              :integer, default: 0, virtual: true
    field :hints,             {:array, :string}, default: []
    field :disabled_hints,    {:array, :string}, default: []
    field :monster,           :any, virtual: true
    field :timers,            :map, virtual: true, default: %{}
    field :admin,             :boolean
    field :inventory,         ApathyDrive.JSONB, default: []
    field :equipment,         ApathyDrive.JSONB, default: []
    field :flags,             :map, default: %{}
    field :monitor_ref,       :any, virtual: true

    timestamps()
  end

  @doc """
  Creates a changeset based on the `model` and `params`.

  If `params` are nil, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(spirit, params \\ %{}) do
    spirit
    |> cast(params, ~w(name class_id)a)
    |> validate_required(~w(name class_id)a)
    |> validate_inclusion(:class_id, ApathyDrive.Class.ids)
    |> validate_format(:name, ~r/^[a-zA-Z]+$/)
    |> unique_constraint(:name, name: :spirits_lower_name_index, on: Repo)
    |> validate_length(:name, min: 1, max: 12)
  end

  def sign_up_changeset(model, params \\ %{}) do
    model
    |> cast(params, ~w(email password)a)
    |> validate_required(~w(email password)a)
    |> validate_format(:email, ~r/@/)
    |> validate_length(:email, min: 3, max: 100)
    |> validate_length(:password, min: 6)
    |> unique_constraint(:email, name: :spirits_lower_email_index, on: Repo)
    |> validate_confirmation(:password)
  end

  def sign_in(email, password) do
    player = Repo.get_by(Spirit, email: email)
    sign_in?(player, password) && player
  end

  def sign_in?(%Spirit{password: stored_hash}, password) do
    checkpw(password, stored_hash)
  end

  def sign_in?(nil, _password) do
    dummy_checkpw()
  end

  def find_or_create_by_external_id(external_id) do
    case Repo.one from s in Spirit, where: s.external_id == ^external_id do
      %Spirit{} = spirit ->
        spirit
      nil ->
        %Spirit{room_id: Room.start_room_id, external_id: external_id}
        |> Repo.insert!
    end
  end

  def save(%Spirit{id: id} = spirit) when is_integer(id) do
    Repo.save!(spirit)
  end
  def save(%Spirit{} = spirit), do: spirit

  def add_experience(%{spirit: nil} = monster, _exp), do: monster
  def add_experience(%{spirit: %Spirit{level: level} = spirit} = monster, exp) do
    spirit =
      spirit
      |> Map.put(:experience, spirit.experience + exp)
      |> ApathyDrive.Level.advance

    if spirit.level > level do
      Mobile.send_scroll monster, "<p>You ascend to level #{spirit.level}!"
    end

    if spirit.level < level do
      Mobile.send_scroll monster, "<p>You fall to level #{spirit.level}!"
    end

    Map.put(monster, :spirit, spirit)
  end

  def online do
    PubSub.subscribers("spirits:online")
  end

end
