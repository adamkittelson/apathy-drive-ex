defmodule Spirit do
  use Ecto.Model
  use GenServer
  use Systems.Reload
  alias ApathyDrive.Repo

  schema "spirits" do
    belongs_to :room, Room
    field :name,              :string
    field :experience,        :integer, default: 0
    field :level,             :integer, default: 1
    field :url,               :string
    field :socket,            :any, virtual: true
    field :idle,              :integer, default: 0, virtual: true
    field :hints,             {:array, :string}, default: []
    field :disabled_hints,    {:array, :string}, default: []
    field :created_at,        :datetime
    field :updated_at,        :datetime
  end

  def create(url) do
    spirit = %Spirit{url: url, room_id: Room.start_room_id, created_at: Ecto.DateTime.utc, updated_at: Ecto.DateTime.utc}
    Repo.insert(spirit)
  end

  def save(spirit_struct) do
    spirit_struct
    |> Repo.update
  end

  def set_room_id(spirit, room_id) do
    GenServer.cast(spirit, {:set_room_id, room_id})
  end


  #############
  # Idle
  #############

  def idle(spirit) do
    GenServer.call(spirit, :idle)
  end

  def increment_idle(spirit) do
    GenServer.cast(spirit, :increment_idle)
  end

  def reset_idle(spirit) do
    GenServer.cast(spirit, :reset_idle)
  end

  ##############
  # Hints
  ##############

  def hints(spirit) do
    GenServer.call(spirit, :hints)
  end

  def activate_hint(spirit, hint) do
    GenServer.cast(spirit, {:activate_hint, hint})
  end

  def deactivate_hint(spirit, hint) do
    GenServer.cast(spirit, {:deactivate_hint, hint})
  end

  def find_by_url(url) do
    query = from s in Spirit,
              where: s.url == ^url

    Repo.one(query)
  end

  def value(spirit) do
    GenServer.call(spirit, :value)
  end

  def handle_call(:value, _from, spirit) do
    {:reply, spirit, spirit}
  end

  def handle_call(:idle, _from, spirit) do
    {:reply, spirit.idle, spirit}
  end

  def handle_call(:hints, _from, spirit) do
    {:reply, spirit.hints, spirit}
  end

  def handle_cast({:set_room_id, room_id}, spirit) do
    spirit = Map.put(spirit, :room_id, room_id)
    save(spirit)
    {:noreply, spirit}
  end

  def handle_cast(:increment_idle, spirit) do
    {:noreply, Map.put(spirit, :idle, spirit.idle + 1)}
  end

  def handle_cast(:reset_idle, spirit) do
    {:noreply, Map.put(spirit, :idle, 0)}
  end

  def handle_cast({:activate_hint, hint}, spirit) do
    if hint in spirit.disabled_hints do
      {:noreply, spirit}
    else
      spirit = spirit
               |> Map.put(:hints, [hint | spirit.hints] |> Enum.uniq)
               |> save
      {:noreply, spirit}
    end
  end

  def handle_cast({:deactivate_hint, hint}, spirit) do
    spirit = spirit
             |> Map.put(:hints, List.delete(spirit.hints, hint))
             |> Map.put(:disabled_hints, [hint | spirit.disabled_hints] |> Enum.uniq)
             |> save

    {:noreply, spirit}
  end

end
