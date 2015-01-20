defmodule Spirit do
  use Ecto.Model
  use GenServer
  use Systems.Reload
  require Logger
  alias ApathyDrive.Repo
  alias Phoenix.PubSub

  @idle_threshold 60

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

  def save(spirit) when is_pid(spirit), do: spirit |> value |> save
  def save(spirit) do
    Repo.update(spirit)
  end

  def set_room_id(spirit, room_id) do
    GenServer.cast(spirit, {:set_room_id, room_id})
  end

  def login(spirit_struct) do
    {:ok, spirit} = Supervisor.start_child(:spirit_supervisor, {:"spirit_#{spirit_struct.id}", {Spirit, :start_link, [spirit_struct]}, :permanent, 5000, :worker, [Spirit]})
    PubSub.subscribe(spirit, "spirits:online")
    PubSub.subscribe(spirit, "spirits:hints")
    spirit
  end

  def logout(spirit) do
    save(spirit)
    spirit_to_kill = :"spirit_#{value(spirit).id}"
    Supervisor.terminate_child(:spirit_supervisor, spirit_to_kill)
    Supervisor.delete_child(:spirit_supervisor, spirit_to_kill)
  end

  def start_link(spirit_struct) do
    GenServer.start_link(Spirit, spirit_struct)
  end

  def online do
    PubSub.subscribers("spirits:online")
  end

  def room(spirit) do
    GenServer.call(spirit, :room)
  end


  #############
  # Idle
  #############

  def reset_idle(spirit) do
    GenServer.cast(spirit, :reset_idle)
  end

  def idle?(spirit) do
    GenServer.call(spirit, :idle?)
  end

  ##############
  # Hints
  ##############

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


  # Generate functions from Ecto schema

  fields = Keyword.keys(@assign_fields)

  Enum.each(fields, fn(field) ->
    def unquote(field)(spirit) do
      GenServer.call(spirit, unquote(field))
    end
  end)

  Enum.each(fields, fn(field) ->
    def handle_call(unquote(field), _from, spirit) do
      {:reply, Map.get(spirit, unquote(field)), spirit}
    end
  end)

  def handle_call(:value, _from, spirit) do
    {:reply, spirit, spirit}
  end

  def handle_call(:idle?, _from, spirit) do
    {:reply, spirit.idle >= @idle_threshold, spirit}
  end

  def handle_call(:room, _from, spirit) do
    {:reply, Room.find(spirit.room_id), spirit}
  end

  def handle_cast({:set_room_id, room_id}, spirit) do
    PubSub.unsubscribe(self, "rooms:#{spirit.room_id}")
    PubSub.subscribe(self, "rooms:#{room_id}")
    spirit = Map.put(spirit, :room_id, room_id)
    save(spirit)
    {:noreply, spirit}
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

  def handle_info(:increment_idle, spirit) do
    {:noreply, Map.put(spirit, :idle, spirit.idle + 1)}
  end

  def handle_info(:display_hint, %{idle: idle} = spirit) when idle >= @idle_threshold, do: {:noreply, spirit}
  def handle_info(:display_hint, %{hints: []}  = spirit), do: {:noreply, spirit}
  def handle_info(:display_hint, spirit) do

    hint = Hint.random(spirit.hints)

    Phoenix.Channel.reply spirit.socket, "scroll", %{:html => "<p>\n<span class='yellow'>Hint:</span> <em>#{hint}</em>\n\n<p>"}

    {:noreply, spirit}
  end

  def handle_info(message, spirit) do
    Logger.warn("#{spirit.name} received unexpected message: #{inspect message}")
    {:noreply, spirit}
  end

end
