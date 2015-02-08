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
    field :pid,               :any, virtual: true
    field :idle,              :integer, default: 0, virtual: true
    field :hints,             {:array, :string}, default: []
    field :disabled_hints,    {:array, :string}, default: []
    field :monster,           :any, virtual: true
    field :created_at,        :datetime
    field :updated_at,        :datetime
  end

  def init(spirit) do
    {:ok, Map.put(spirit, :pid, self)}
  end

  def create(url) do
    spirit = %Spirit{url: url, room_id: Room.start_room_id, created_at: Ecto.DateTime.utc, updated_at: Ecto.DateTime.utc}
    Repo.insert(spirit)
  end

  def save(spirit) when is_pid(spirit), do: spirit |> value |> save
  def save(%Spirit{id: id} = spirit) when is_integer(id) do
    Repo.update(spirit)
  end
  def save(%Spirit{} = spirit), do: spirit

  def execute_command(spirit, command, arguments) do
    GenServer.cast(spirit, {:execute_command, command, arguments})
  end

  def login(%Spirit{} = spirit) do
    {:ok, pid} = Supervisor.start_child(ApathyDrive.Supervisor, {:"spirit_#{spirit.id}", {Spirit, :start_link, [spirit]}, :permanent, 5000, :worker, [Spirit]})
    PubSub.subscribe(pid, "spirits:online")
    PubSub.subscribe(pid, "spirits:hints")
    PubSub.subscribe(pid, "rooms:#{spirit.room_id}")
    pid
  end

  def deactivate_hint(%Spirit{} = spirit, hint) do
    spirit
    |> Map.put(:hints, List.delete(spirit.hints, hint))
    |> Map.put(:disabled_hints, [hint | spirit.disabled_hints] |> Enum.uniq)
  end

  def set_room_id(%Spirit{} = spirit, room_id) do
    PubSub.unsubscribe(self, "rooms:#{spirit.room_id}")
    PubSub.subscribe(self, "rooms:#{room_id}")
    Map.put(spirit, :room_id, room_id)
  end

  def find_room(%Spirit{room_id: room_id} = spirit) do
    room_id
    |> Room.find
    |> Room.value
  end

  def send_disable(%Spirit{socket: socket} = spirit, elem) do
    Phoenix.Channel.reply socket, "disable", %{:html => elem}
    spirit
  end

  def send_focus(%Spirit{socket: socket} = spirit, elem) do
    Phoenix.Channel.reply socket, "focus", %{:html => elem}
    spirit
  end

  def send_up(%Spirit{socket: socket} = spirit) do
    Phoenix.Channel.reply socket, "up", %{}
    spirit
  end

  def send_scroll(%Spirit{socket: socket} = spirit, html) do
    Phoenix.Channel.reply socket, "scroll", %{:html => html}
    spirit
  end

  def send_update_prompt(%Spirit{socket: socket} = spirit, html) do
    Phoenix.Channel.reply socket, "update prompt", %{:html => html}
    spirit
  end

  def logout(spirit) do
    save(spirit)
    spirit_to_kill = :"spirit_#{value(spirit).id}"
    Supervisor.terminate_child(ApathyDrive.Supervisor, spirit_to_kill)
    Supervisor.delete_child(ApathyDrive.Supervisor, spirit_to_kill)
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
    def unquote(field)(pid) do
      GenServer.call(pid, unquote(field))
    end

    def unquote(field)(pid, new_value) do
      GenServer.call(pid, {unquote(field), new_value})
    end
  end)

  Enum.each(fields, fn(field) ->
    def handle_call(unquote(field), _from, state) do
      {:reply, Map.get(state, unquote(field)), state}
    end

    def handle_call({unquote(field), new_value}, _from, state) do
      {:reply, new_value, Map.put(state, unquote(field), new_value)}
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

  def handle_cast({:execute_command, command, arguments}, spirit) do
    spirit = ApathyDrive.Command.execute(spirit, command, arguments)
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

  def handle_info({:socket_broadcast, message}, spirit) do
    Phoenix.Channel.reply spirit.socket, message.event, message.payload

    {:noreply, spirit}
  end

  def handle_info(:unpossess, spirit) do
    monster = spirit.monster

    spirit = spirit
             |> Map.put(:monster, nil)
             |> Spirit.send_scroll("<p>You leave the body of #{Monster.name(monster)}.</p>")
             |> Systems.Prompt.update

    Phoenix.PubSub.unsubscribe(self, "monsters:#{Monster.id(monster)}")

    {:noreply, spirit}
  end

  def handle_info(_message, spirit) do
    {:noreply, spirit}
  end

end
