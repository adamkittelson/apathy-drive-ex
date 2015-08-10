defmodule Spirit do
  use GenServer
  use ApathyDrive.Web, :model

  require Logger
  import Systems.Text
  import Comeonin.Bcrypt
  alias ApathyDrive.PubSub

  @idle_threshold 60

  schema "spirits" do
    belongs_to :room, Room
    field :name,              :string
    field :alignment,         :string
    field :email,             :string
    field :password,          :string
    field :external_id,       :string
    field :experience,        :integer, default: 0
    field :level,             :integer, default: 1
    field :faction,           :string
    field :socket,            :any, virtual: true
    field :socket_pid,        :any, virtual: true
    field :pid,               :any, virtual: true
    field :idle,              :integer, default: 0, virtual: true
    field :hints,             {:array, :string}, default: []
    field :disabled_hints,    {:array, :string}, default: []
    field :monster,           :any, virtual: true
    field :abilities,         :any, virtual: true
    field :max_mana,          :integer, virtual: true
    field :mana,              :integer, virtual: true
    field :mana_regen,        :integer, virtual: true
    field :timers,            :any,     virtual: true, default: %{}
    field :admin,             :boolean

    timestamps
  end

  def init(%Spirit{} = spirit) do
    send(self, :set_abilities)

    spirit =
      spirit |> TimerManager.call_every({:regen_mana, 10_000, fn -> send(self, :regen_mana) end})

    {:ok, Map.put(spirit, :pid, self)}
  end

  @doc """
  Creates a changeset based on the `model` and `params`.

  If `params` are nil, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(spirit, params \\ :empty) do
    spirit
    |> cast(params, ~w(name faction alignment), ~w())
    |> validate_inclusion(:faction,   ["Angel", "Demon", "Elemental"])
    |> validate_inclusion(:alignment, ["good", "neutral", "evil"])
    |> validate_format(:name, ~r/^[a-zA-Z]+$/)
    |> validate_unique(:name, on: Repo)
    |> validate_length(:name, min: 1, max: 18)
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
    player = Repo.get_by(Spirit, email: email)
    sign_in?(player, password) && player
  end

  def sign_in?(%Spirit{password: stored_hash}, password) do
    checkpw(password, stored_hash)
  end

  def sign_in?(nil, _password) do
    dummy_checkpw
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

  def save(spirit) when is_pid(spirit), do: spirit |> value |> save
  def save(%Spirit{id: id} = spirit) when is_integer(id) do
    Repo.update!(spirit)
  end
  def save(%Spirit{} = spirit), do: spirit

  def alignment_color(%{alignment: "evil"}),    do: "magenta"
  def alignment_color(%{alignment: "good"}),    do: "white"
  def alignment_color(%{alignment: "neutral"}), do: "dark-cyan"

  def execute_command(%Spirit{pid: pid}, command, arguments) do
    GenServer.call(pid, {:execute_command, command, arguments})
  end

  def set_abilities(%Spirit{} = spirit) do
    abilities = Ability.trainable(spirit)
                |> Enum.reject(fn(%Ability{} = ability) ->
                     case spirit.alignment do
                       "good" ->
                         Enum.member?(ability.flags, "neutral") or Enum.member?(ability.flags, "evil") or Enum.member?(ability.flags, "not-good")
                       "neutral" ->
                         Enum.member?(ability.flags, "good") or Enum.member?(ability.flags, "evil") or Enum.member?(ability.flags, "not-neutral")
                       "evil" ->
                         Enum.member?(ability.flags, "good") or Enum.member?(ability.flags, "neutral") or Enum.member?(ability.flags, "not-evil")
                     end
                   end)

    spirit
    |> Map.put(:abilities, abilities)
    |> set_max_mana
    |> set_mana
    |> set_mana_regen
  end

  def set_max_mana(%Spirit{abilities: abilities} = spirit) do
    max_mana = Enum.reduce(abilities, 0, fn(ability, max_mana) ->
      max_mana + Map.get(ability.properties, "mana_cost", 0)
    end)
    Map.put(spirit, :max_mana, max_mana * 2)
  end

  def set_mana(%Spirit{mana: nil, max_mana: max_mana} = spirit) do
    spirit
    |> Map.put(:mana, max_mana)
    |> Systems.Prompt.update
  end
  def set_mana(%Spirit{mana: mana, max_mana: max_mana} = spirit) do
    spirit
    |> Map.put(:mana, min(mana, max_mana))
    |> Systems.Prompt.update
  end

  def set_mana_regen(%Spirit{max_mana: max_mana} = spirit) do
    Map.put(spirit, :mana_regen,  max_mana |> div(10) |> max(1))
  end

  def login(%Spirit{} = spirit) do
    spirit = case Supervisor.start_child(ApathyDrive.Supervisor, {:"spirit_#{spirit.id}", {Spirit, :start_link, [spirit]}, :transient, 5000, :worker, [Spirit]}) do
      {:ok, pid} ->
        Map.put(spirit, :pid, pid)
      {:error, {:already_started, _}} ->
        spirit
      {:error, :already_present} ->
        Supervisor.delete_child(ApathyDrive.Supervisor, :"spirit_#{spirit.id}")
        login(spirit)
    end

    PubSub.subscribe(spirit.pid, "spirits:online")
    PubSub.subscribe(spirit.pid, "spirits:hints")
    PubSub.subscribe(spirit.pid, "spirits:#{spirit.faction}")
    PubSub.subscribe(spirit.pid, "chat:gossip")
    PubSub.subscribe(spirit.pid, "chat:#{spirit.faction}")
    PubSub.subscribe(spirit.pid, "rooms:#{spirit.room_id}")
    #ApathyDrive.WhoList.log_on(spirit)

    spirit
  end

  def activate_hint(%Spirit{} = spirit, hint) do
    if hint in spirit.disabled_hints do
      spirit
    else
      spirit
      |> Map.put(:hints, [hint | spirit.hints] |> Enum.uniq)
      |> save
    end
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

  def find_room(%Spirit{room_id: room_id}) do
    room_id
    |> Room.find
    |> Room.value
  end

  def send_disable(%Spirit{socket: socket} = spirit, elem) do
    Phoenix.Channel.push socket, "disable", %{:html => elem}
    spirit
  end

  def send_focus(%Spirit{socket: socket} = spirit, elem) do
    Phoenix.Channel.push socket, "focus", %{:html => elem}
    spirit
  end

  def send_up(%Spirit{socket: socket} = spirit) do
    Phoenix.Channel.push socket, "up", %{}
    spirit
  end

  def send_scroll(%Spirit{socket: socket} = spirit, html) do
    Phoenix.Channel.push socket, "scroll", %{:html => html}
    spirit
  end

  def send_update_prompt(%Spirit{socket: socket} = spirit, html) do
    Phoenix.Channel.push socket, "update prompt", %{:html => html}
    spirit
  end

  def add_experience(%Spirit{} = spirit, exp) do
    spirit = spirit
             |> Map.put(:experience, spirit.experience + exp)
             |> Systems.Level.advance
             |> Spirit.save

    spirit
  end

  def logout(%Spirit{} = spirit) do
    spirit
    |> save
    #|> ApathyDrive.WhoList.log_off

    PubSub.unsubscribe(spirit.pid, "spirits:online")
    PubSub.unsubscribe(spirit.pid, "spirits:hints")
    PubSub.unsubscribe(spirit.pid, "spirits:#{spirit.faction}")
    PubSub.unsubscribe(spirit.pid, "chat:gossip")
    PubSub.unsubscribe(spirit.pid, "chat:#{spirit.faction}")
    PubSub.unsubscribe(spirit.pid, "rooms:#{spirit.room_id}")
  end

  def start_link(spirit_struct) do
    GenServer.start_link(Spirit, spirit_struct, [name: {:global, :"spirit_#{spirit_struct.id}"}])
  end

  def online do
    PubSub.subscribers("spirits:online")
  end

  def room(spirit) do
    GenServer.call(spirit, :room)
  end

  def update_socket(spirit_pid, socket, socket_pid) do
    GenServer.call(spirit_pid, {:update_socket, socket, socket_pid})
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

  def value(spirit) do
    GenServer.call(spirit, :value)
  end


  # Generate functions from Ecto schema
  fields = Keyword.keys(@struct_fields) -- Keyword.keys(@ecto_assocs)

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

  def handle_call({:update_socket, socket, socket_pid}, _from, spirit) do
    spirit =
      spirit
      |> Map.put(:socket, socket)
      |> Map.put(:socket_pid, socket_pid)
    {:reply, spirit, spirit}
  end

  def handle_call({:execute_command, command, arguments}, _from, spirit) do
    try do
      case ApathyDrive.Command.execute(spirit, command, arguments) do
        %Spirit{} = spirit ->
          {:reply, spirit, spirit}
        %Monster{} = monster ->
          {:reply, monster, spirit}
      end
    catch
      kind, error ->
        Spirit.send_scroll(spirit, "<p><span class='red'>Something went wrong.</span></p>")
        IO.puts Exception.format(kind, error)
        {:reply, spirit, spirit}
    end
  end

  def handle_cast(:reset_idle, spirit) do
    {:noreply, Map.put(spirit, :idle, 0)}
  end

  def handle_info(:increment_idle, spirit) do
    {:noreply, Map.put(spirit, :idle, spirit.idle + 1)}
  end

  def handle_info(:display_hint, %{idle: idle} = spirit) when idle >= @idle_threshold, do: {:noreply, spirit}
  def handle_info(:display_hint, %{hints: []}  = spirit), do: {:noreply, spirit}
  def handle_info(:display_hint, spirit) do

    hint = Hint.random(spirit.hints)

    if hint do
      Phoenix.Channel.push spirit.socket, "scroll", %{:html => "<p>\n<span class='yellow'>Hint:</span> <em>#{hint}</em>\n\n<p>"}
    end

    {:noreply, spirit}
  end

  def handle_info({:socket_broadcast, message}, spirit) do
    Phoenix.Channel.push spirit.socket, message.event, message.payload

    {:noreply, spirit}
  end

  def handle_info({:possessed_monster_died, %Monster{} = monster}, spirit) do
    room_id = monster.room_id

    spirit = spirit
             |> Map.put(:monster, nil)
             |> set_room_id(room_id)
             |> Spirit.send_scroll("<p>You leave the body of #{monster.name}.</p>")
             |> Systems.Prompt.update

    ApathyDrive.PubSub.unsubscribe(self, "monsters:#{monster.id}")

    {:noreply, spirit}
  end

  def handle_info({:greet, %{greeter: greeter, greeted: greeted}}, spirit) do
    send_scroll(spirit, "<p><span class='dark-green'>#{greeter.name |> capitalize_first} greets #{greeted.name}.</span></p>")
    {:noreply, spirit}
  end

  def handle_info({:door_bashed_open, %{basher: %Monster{} = basher,
                                        direction: direction,
                                        type: type}},
                                        spirit) do

    send_scroll(spirit, "<p>You see #{basher.name} bash open the #{type} #{ApathyDrive.Exit.direction_description(direction)}.</p>")
    {:noreply, spirit}
  end

  def handle_info({:mirror_bash, room_exit}, spirit) do
    send_scroll(spirit, "<p>The #{String.downcase(room_exit["kind"])} #{ApathyDrive.Exit.direction_description(room_exit["direction"])} just flew open!</p>")
    {:noreply, spirit}
  end

  def handle_info({:door_bash_failed, %{basher: %Monster{} = basher,
                                        direction: direction,
                                        type: type}},
                                        spirit) do

    send_scroll(spirit, "<p>You see #{basher.name} attempt to bash open the #{type} #{ApathyDrive.Exit.direction_description(direction)}.</p>")
    {:noreply, spirit}
  end

  def handle_info({:mirror_bash_failed, room_exit}, spirit) do
    send_scroll(spirit, "<p>The #{String.downcase(room_exit["kind"])} #{ApathyDrive.Exit.direction_description(room_exit["direction"])} shudders from an impact, but it holds!</p>")
    {:noreply, spirit}
  end

  def handle_info({:door_opened, %{opener: %Monster{} = opener,
                                   direction: direction,
                                   type: type}},
                                   spirit) do

    send_scroll(spirit, "<p>You see #{opener.name} open the #{type} #{ApathyDrive.Exit.direction_description(direction)}.</p>")
    {:noreply, spirit}
  end

  def handle_info({:mirror_open, room_exit}, spirit) do
    send_scroll(spirit, "<p>The #{String.downcase(room_exit["kind"])} #{ApathyDrive.Exit.direction_description(room_exit["direction"])} just opened.</p>")
    {:noreply, spirit}
  end

  def handle_info({:door_closed, %{closer: %Monster{} = closer,
                                   direction: direction,
                                   type: type}},
                                   spirit) do

    send_scroll(spirit, "<p>You see #{closer.name} close the #{type} #{ApathyDrive.Exit.direction_description(direction)}.</p>")
    {:noreply, spirit}
  end

  def handle_info({:mirror_close, room_exit}, spirit) do
    send_scroll(spirit, "<p>The #{String.downcase(room_exit["kind"])} #{ApathyDrive.Exit.direction_description(room_exit["direction"])} just closed.</p>")
    {:noreply, spirit}
  end

  def handle_info({:door_locked, %{locker: %Monster{} = locker,
                                   direction: direction,
                                   type: type}},
                                   spirit) do

    send_scroll(spirit, "<p>You see #{locker.name} lock the #{type} #{ApathyDrive.Exit.direction_description(direction)}.</p>")
    {:noreply, spirit}
  end

  def handle_info({:mirror_lock, room_exit}, spirit) do
    send_scroll(spirit, "<p>The #{String.downcase(room_exit["kind"])} #{ApathyDrive.Exit.direction_description(room_exit["direction"])} just locked!</p>")
    {:noreply, spirit}
  end

  def handle_info({:cast_message, messages: messages,
                                  user: %Monster{},
                                  target: %Monster{}},
                  %Spirit{} = spirit) do


    if messages["spectator"] do
      send_scroll(spirit, messages["spectator"])
    end

    {:noreply, spirit}
  end

  def handle_info({:monster_died, monster: %Monster{} = deceased, reward: _exp}, spirit) do
    message = deceased.death_message
              |> interpolate(%{"name" => deceased.name})
              |> capitalize_first

    Spirit.send_scroll(spirit, "<p>#{message}</p>")

    {:noreply, spirit}
  end

  def handle_info({:monster_dodged, messages: messages,
                                    user: %Monster{} = user,
                                    target: %Monster{} = target},
                  spirit) do

    message = interpolate(messages["spectator"], %{"user" => user, "target" => target})
    send_scroll(spirit, "<p><span class='dark-cyan'>#{message}</span></p>")

    {:noreply, spirit}
  end

  def handle_info(:set_abilities, spirit) do
    {:noreply, set_abilities(spirit) }
  end

  def handle_info(:display_prompt, spirit) do
    {:noreply, Systems.Prompt.display(spirit) }
  end

  def handle_info({:gossip, name, message}, spirit) do
    Spirit.send_scroll(spirit, "<p>[<span class='dark-magenta'>gossip</span> : #{name}] #{message}</p>")
    {:noreply, spirit}
  end

  def handle_info({:angel, name, message}, spirit) do
    Spirit.send_scroll(spirit, "<p>[<span class='white'>angel</span> : #{name}] #{message}</p>")
    {:noreply, spirit}
  end

  def handle_info({:elemental, name, message}, spirit) do
    Spirit.send_scroll(spirit, "<p>[<span class='dark-cyan'>elemental</span> : #{name}] #{message}</p>")
    {:noreply, spirit}
  end

  def handle_info({:demon, name, message}, spirit) do
    Spirit.send_scroll(spirit, "<p>[<span class='magenta'>demon</span> : #{name}] #{message}</p>")
    {:noreply, spirit}
  end

  def handle_info(:go_away, spirit) do
    save(spirit)
    Process.exit(self, :normal)
    {:noreply, spirit}
  end

  def handle_info({:lair_control_reward, count, bonus}, spirit) do
    exp = count * spirit.level + bonus

    spirit =
      spirit
      |> Spirit.add_experience(exp)
      |> send_scroll("<p>You gain #{exp} bonus experience!<br><br></p>")

    {:noreply, spirit}
  end

  def handle_info(:regen_mana, %Spirit{mana: mana, max_mana: max_mana, mana_regen: mana_regen} = spirit) do
    spirit = spirit
             |> Map.put(:mana, min(mana + mana_regen, max_mana))
             |> Systems.Prompt.update

    {:noreply, spirit}
  end

  def handle_info({:timeout, _ref, {name, time, function}}, %Spirit{timers: timers} = spirit) do
    jitter = trunc(time / 2) + :random.uniform(time)

    new_ref = :erlang.start_timer(jitter, self, {name, time, function})

    timers = Map.put(timers, name, new_ref)

    TimerManager.execute_function(function)

    {:noreply, Map.put(spirit, :timers, timers)}
  end

  def handle_info({:timeout, _ref, {name, function}}, %Spirit{timers: timers} = spirit) do
    TimerManager.execute_function(function)

    timers = Map.delete(timers, name)

    {:noreply, Map.put(spirit, :timers, timers)}
  end

  def handle_info({:reroll, name: name, faction: faction, alignment: alignment}, spirit) do
    new_exp = div(spirit.experience, 10)

    spirit =
     spirit
     |> Map.put(:name, name)
     |> Map.put(:faction, faction)
     |> Map.put(:alignment, alignment)
     |> Map.put(:experience, new_exp)
     |> Map.put(:level, Systems.Level.level_at_exp(new_exp))
     |> set_abilities
     |> Spirit.save

     #ApathyDrive.WhoList.log_on(spirit)

    {:noreply, spirit}
  end

  def handle_info(_message, spirit) do
    {:noreply, spirit}
  end

end
