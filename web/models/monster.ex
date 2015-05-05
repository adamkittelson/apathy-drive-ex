defmodule Monster do
  require Logger
  use Ecto.Model
  use GenServer

  import Systems.Text
  alias ApathyDrive.Repo
  alias ApathyDrive.PubSub

  schema "monsters" do
    field :name,                :string
    field :lair_id,             :integer
    field :skills,              :any,     virtual: true
    field :level,               :integer, virtual: true
    field :alignment,           :string,  virtual: true
    field :experience,          :integer, virtual: true
    field :max_hp,              :integer, virtual: true
    field :max_mana,            :integer, virtual: true
    field :hp,                  :integer, virtual: true
    field :mana,                :integer, virtual: true
    field :hp_regen,            :integer, virtual: true
    field :mana_regen,          :integer, virtual: true
    field :hunting,             :any,     virtual: true, default: []
    field :combat,              :any,     virtual: true, default: %{"break_at" => 0}
    field :effects,             :any,     virtual: true, default: %{}
    field :timers,              :any,     virtual: true, default: %{}
    field :disposition,         :string,  virtual: true
    field :description,         :string,  virtual: true
    field :death_message,       :string,  virtual: true
    field :enter_message,       :string,  virtual: true
    field :exit_message,        :string,  virtual: true
    field :abilities,           :any,     virtual: true
    field :greeting,            :string,  virtual: true
    field :gender,              :string,  virtual: true
    field :chance_to_follow,    :integer, virtual: true
    field :questions,           :any,     virtual: true
    field :pid,                 :any,     virtual: true
    field :keywords,            {:array, :string}, virtual: true
    field :flags,               {:array, :string}, virtual: true
    field :hate,                :any, virtual: true, default: HashDict.new
    field :attacks,             :any, virtual: true
    field :spirit,              :any, virtual: true

    timestamps

    belongs_to :room, Room
    belongs_to :monster_template, MonsterTemplate
  end

  def init(%Monster{} = monster) do
    :random.seed(:os.timestamp)

    if monster.room_id do
      PubSub.subscribe(self, "rooms:#{monster.room_id}")
      PubSub.subscribe(self, "rooms:#{monster.room_id}:monsters")
      PubSub.subscribe(self, "rooms:#{monster.room_id}:monsters:#{monster_alignment(monster)}")
    end

    if monster.lair_id do
      PubSub.subscribe(self, "rooms:#{monster.lair_id}:spawned_monsters")
    end

    PubSub.subscribe(self, "monsters")
    PubSub.subscribe(self, "monster_template:#{monster.monster_template_id}")

    monster = monster
              |> Map.put(:pid, self)

    :global.register_name(:"monster_#{monster.id}", self)
    Process.register(self, :"monster_#{monster.id}")

    send(self, :set_abilities)

    :ets.new(:"monster_#{monster.id}", [:named_table, :set, :public])

    :ets.insert(:"monster_#{monster.id}", {self, monster})

    monster = monster
              |> TimerManager.call_every({:periodic_effects, 5_000, fn -> send(self, :apply_periodic_effects) end})
              |> TimerManager.call_every({:monster_ai,       5_000, fn -> send(self, :think) end})
              |> TimerManager.call_every({:monster_regen,   10_000, fn -> send(self, :regen) end})
              |> TimerManager.call_every({:calm_down,       10_000, fn -> send(self, :calm_down) end})

    {:ok, monster}
  end

  def held(%Monster{effects: effects} = monster) do
    effects
    |> Map.values
    |> Enum.find(fn(effect) ->
         Map.has_key?(effect, "held")
       end)
    |> held(monster)
  end
  def held(nil, %Monster{}), do: false
  def held(%{"effect_message" => message}, %Monster{} = monster) do
    send_scroll(monster, "<p>#{message}</p>")
  end

  def silenced(%Monster{effects: effects} = monster, %Ability{properties: %{"mana_cost" => cost}}) when cost > 0 do
    effects
    |> Map.values
    |> Enum.find(fn(effect) ->
         Map.has_key?(effect, "silenced")
       end)
    |> silenced(monster)
  end
  def silenced(%Monster{}, %Ability{}), do: false
  def silenced(nil,        %Monster{}), do: false
  def silenced(%{"effect_message" => message}, %Monster{} = monster) do
    send_scroll(monster, "<p>#{message}</p>")
  end

  def confuse(%Monster{effects: effects} = monster) do
    effects
    |> Map.values
    |> Enum.find(fn(effect) ->
         Map.has_key?(effect, "confused") && (effect["confused"] >= :random.uniform(100))
       end)
    |> confuse(monster)
  end
  def confuse(nil, %Monster{}), do: false
  def confuse(%{"confusion_message" => %{"user" => user_message, "spectator" => spectator_message}}, %Monster{} = monster) do
    send_scroll(monster, "<p>#{user_message}</p>")
    ApathyDrive.Endpoint.broadcast_from! self, "rooms:#{monster.room_id}", "scroll", %{:html => "<p>#{interpolate(spectator_message, %{"user" => monster})}</p>"}
  end
  def confuse(%{}, %Monster{} = monster) do
    send_scroll(monster, "<p>You fumble in confusion!</p>")
    ApathyDrive.Endpoint.broadcast_from! self, "rooms:#{monster.room_id}", "scroll", %{:html => "<p>#{interpolate("{{user}} fumbles in confusion!", %{"user" => monster})}</p>"}
  end

  def set_abilities(%Monster{monster_template_id: nil} = monster) do
    monster
  end
  def set_abilities(%Monster{} = monster) do
    abilities = monster_template_abilities(monster)

    abilities = abilities ++
                abilities_from_attacks(monster) ++
                abilities_from_spirit(monster)

    monster
    |> Map.put(:abilities, abilities)
  end

  def heal_abilities(%Monster{abilities: abilities} = monster) do
    abilities
    |> Enum.filter(&(&1.kind == "heal"))
    |> Ability.useable(monster)
  end

  def bless_abilities(%Monster{abilities: abilities} = monster) do
    abilities
    |> Enum.filter(&(&1.kind == "blessing"))
    |> Ability.useable(monster)
    |> Enum.reject(fn(ability) ->
         Ability.removes_blessing?(monster, ability)
       end)
  end

  def attack_abilities(%Monster{abilities: abilities} = monster) do
    abilities
    |> Enum.filter(&(&1.kind == "attack" and &1.name != "attack"))
    |> Ability.useable(monster)
  end

  def monster_attacks(%Monster{abilities: abilities} = monster) do
    abilities
    |> Enum.filter(&(&1.name == "attack"))
    |> Ability.useable(monster)
  end

  def monster_template_abilities(%Monster{} = monster) do
    mt = monster.monster_template_id
         |> MonsterTemplate.find
         |> MonsterTemplate.value

    mt.abilities
    |> Enum.map(&(Repo.get(Ability, &1)))
  end

  def abilities_from_spirit(%Monster{spirit: %Spirit{abilities: abilities}}), do: abilities
  def abilities_from_spirit(%Monster{}), do: []

  def abilities_from_attacks(%Monster{attacks: []}) do
    [
      %Ability{
        name:    "attack",
        command: "a",
        kind:    "attack",
        global_cooldown: 4,
        level: 0,
        flags: [],
        properties: %{
          "dodgeable" => true,
          "accuracy_skill" => "attack",
          "dodge_message" => %{
            "target" => "You dodge {{user}}'s attack!",
            "user" => "{{Target}} dodges your attack!",
            "spectator" => "{{Target}} dodges {{user}}'s attack!"
          },
          "instant_effects" => %{
            "damage" => %{
              "scaling" => %{
                "attack" => %{
                  "max_every"    => 20,
                  "max_increase" => 1,
                  "min_every"    => 25,
                  "min_increase" => 1
                }
              },
              "base_min" => 2,
              "base_max" => 6
            }
          },
          "cast_message" => %{
            "target" => "{{user}} hits you for {{amount}} damage!",
            "user" => "You hit {{target}} for {{amount}} damage!",
            "spectator" => "{{user}} hits {{target}} for {{amount}} damage!"
          },
          "damage_type" => "normal"
        }
      }
    ]
  end
  def abilities_from_attacks(%Monster{attacks: attacks}) do
    Enum.map(attacks, fn(attack) ->
      %Ability{
        name:    attack["name"],
        command: attack["command"],
        kind:    attack["kind"],
        global_cooldown: attack["global_cooldown"],
        flags: attack["flags"],
        properties: attack["properties"]
      }
    end)
  end

  def on_cooldown?(%Monster{} = monster, %Ability{name: "attack"} = ability) do
    on_attack_cooldown?(monster, ability)
  end
  def on_cooldown?(%Monster{} = monster, %Ability{} = ability) do
    on_global_cooldown?(monster, ability)
  end

  def on_attack_cooldown?(%Monster{},           %Ability{global_cooldown: nil}), do: false
  def on_attack_cooldown?(%Monster{} = monster, %Ability{global_cooldown: _}), do: on_attack_cooldown?(monster)
  def on_attack_cooldown?(%Monster{effects: effects}) do
    effects
    |> Map.values
    |> Enum.any?(&(&1["cooldown"] == :attack))
  end

  def on_global_cooldown?(%Monster{},           %Ability{global_cooldown: nil}), do: false
  def on_global_cooldown?(%Monster{} = monster, %Ability{global_cooldown: _}), do: on_global_cooldown?(monster)
  def on_global_cooldown?(%Monster{effects: effects}) do
    effects
    |> Map.values
    |> Enum.any?(&(&1["cooldown"] == :global))
  end

  def on_ai_move_cooldown?(%Monster{effects: effects}) do
    effects
    |> Map.values
    |> Enum.any?(&(&1["cooldown"] == :ai_movement))
  end

  def blind?(%Monster{effects: effects}) do
    effects
    |> Map.values
    |> Enum.any?(&(Map.has_key?(&1, "blinded")))
  end

  def execute_command(%Monster{pid: pid}, command, arguments) do
    GenServer.call(pid, {:execute_command, command, arguments})
  end

  def possess(monster, %Spirit{} = spirit) do
    GenServer.call(monster, {:possess, spirit})
  end

  def value(monster_pid) do
    {:registered_name, table} = Process.info(monster_pid, :registered_name)

    [{^monster_pid, %Monster{} = monster}] = :ets.lookup(table, monster_pid)
    monster
  end

  def insert(%Monster{id: nil} = monster) do
    ApathyDrive.Repo.insert(monster)
  end
  def insert(%Monster{} = monster), do: monster

  def save(%Monster{id: id, spirit: %Spirit{} = spirit} = monster) when is_integer(id) do
    spirit = Spirit.save(spirit)
    monster = monster
              |> Map.put(:spirit, spirit)
              |> Repo.update
    :ets.insert(:"monster_#{id}", {self, monster})
    monster
  end
  def save(%Monster{id: id} = monster) when is_integer(id) do
    monster = Repo.update(monster)
    :ets.insert(:"monster_#{id}", {self, monster})
    monster
  end
  def save(%Monster{} = monster), do: monster

  def find(id) do
    case :global.whereis_name(:"monster_#{id}") do
      :undefined ->
        load(id)
      monster ->
        monster
    end
  end

  def load(id) do
    case Repo.one from m in Monster, where: m.id == ^id do
      %Monster{} = monster ->

        mt = monster.monster_template_id
             |> MonsterTemplate.find
             |> MonsterTemplate.value

        monster = Map.merge(mt, monster, fn(_key, mt_val, monster_val) ->
                    monster_val || mt_val
                  end)
                  |> Map.from_struct
                  |> Map.delete(:__meta__)
                  |> Enum.into(Keyword.new)

        monster = struct(Monster, monster)

        monster = monster
                  |> Map.put(:hp, monster.max_hp)
                  |> Map.put(:mana, monster.max_mana)
                  |> Map.put(:keywords, String.split(monster.name))
                  |> Map.put(:effects, %{"monster_template" => mt.effects})

        {:ok, pid} = Supervisor.start_child(ApathyDrive.Supervisor, {:"monster_#{monster.id}", {GenServer, :start_link, [Monster, monster, []]}, :transient, 5000, :worker, [Monster]})

        pid
      nil ->
        nil
    end
  end

  def find_room(%Monster{room_id: room_id}) do
    room_id
    |> Room.find
    |> Room.value
  end

  def set_room_id(%Monster{} = monster, room_id) do
    PubSub.unsubscribe(self, "rooms:#{monster.room_id}")
    PubSub.unsubscribe(self, "rooms:#{monster.room_id}:monsters")
    PubSub.unsubscribe(self, "rooms:#{monster.room_id}:monsters:#{monster_alignment(monster)}")

    PubSub.subscribe(self, "rooms:#{room_id}")
    PubSub.subscribe(self, "rooms:#{room_id}:monsters")
    PubSub.subscribe(self, "rooms:#{room_id}:monsters:#{monster_alignment(monster)}")

    monster
    |> Map.put(:room_id, room_id)
  end

  def effect_description(%Monster{effects: effects}) do
    effects
    |> Map.values
    |> Enum.find(fn(effect) ->
         Map.has_key?(effect, "description")
       end)
    |> effect_description
  end

  def effect_description(nil), do: nil
  def effect_description(%{"description" => description}), do: description

  def max_mana(%Monster{max_mana: max_mana}) do
    max_mana
  end

  def effect_bonus(%Monster{effects: effects}, name) do
    effects
    |> Map.values
    |> Enum.map(fn
         (%{} = effect) ->
           Map.get(effect, name, 0)
         (_) ->
           0
       end)
    |> Enum.sum
  end

  def base_skills(%Monster{skills: skills} = monster) do
    skills
    |> Map.keys
    |> Enum.reduce(%{}, fn(skill_name, base_skills) ->
         Map.put(base_skills, skill_name, base_skill(monster, skill_name))
       end)
  end

  def base_skill(%Monster{skills: skills}, skill_name) do
    Map.get(skills, skill_name, 0)
  end

  def modified_skill(%Monster{} = monster, skill_name) do
    base_skill(monster, skill_name) + effect_bonus(monster, skill_name)
  end

  def send_scroll(monster, html) when is_pid(monster) do
    send(monster, {:scroll, html})
  end
  def send_scroll(%Monster{spirit: %Spirit{socket: socket}} = monster, html) do
    Phoenix.Channel.push socket, "scroll", %{:html => html}
    monster
  end
  def send_scroll(%Monster{spirit: nil} = monster, _html), do: monster

  def send_disable(%Monster{spirit: %Spirit{socket: socket}} = monster, elem) do
    Phoenix.Channel.push socket, "disable", %{:html => elem}
    monster
  end
  def send_disable(%Monster{spirit: nil} = monster, _html), do: monster

  def send_focus(%Monster{spirit: %Spirit{socket: socket}} = monster, elem) do
    Phoenix.Channel.push socket, "focus", %{:html => elem}
    monster
  end
  def send_focus(%Monster{spirit: nil} = monster, _html), do: monster

  def send_up(%Monster{spirit: %Spirit{socket: socket}} = monster) do
    Phoenix.Channel.push socket, "up", %{}
    monster
  end
  def send_up(%Monster{spirit: nil} = monster), do: monster

  def send_update_prompt(%Monster{spirit: %Spirit{socket: socket}} = monster, html) do
    Phoenix.Channel.push socket, "update prompt", %{:html => html}
    monster
  end
  def send_update_prompt(%Monster{spirit: nil} = monster, _html), do: monster

  def look_name(%Monster{} = monster) do
    "<span class='#{alignment_color(monster)}'>#{monster.name}</span>"
  end

  def alignment_color(%Monster{} = monster) do
    case monster_alignment(monster) do
      "evil" ->
        "magenta"
      "good" ->
        "white"
      "neutral" ->
        "dark-cyan"
    end
  end

  def monster_alignment(%Monster{spirit: %Spirit{alignment: alignment}}) do
    alignment
  end
  def monster_alignment(%Monster{alignment: alignment}) do
    alignment
  end

  def display_enter_message(%Room{} = room, monster) when is_pid(monster) do
    display_enter_message(%Room{} = room, Monster.value(monster))
  end
  def display_enter_message(%Room{} = room, %Monster{} = monster) do
    display_enter_message(room, monster, Room.random_direction(room))
  end

  def display_enter_message(%Room{} = room, monster, _direction)  when is_pid(monster) do
    display_enter_message(room, Monster.value(monster), Room.random_direction(room))
  end
  def display_enter_message(%Room{} = room, %Monster{enter_message: enter_message, name: name} = monster, direction) do
    message = enter_message
              |> interpolate(%{
                   "name" => name,
                   "direction" => Room.enter_direction(direction),
                   "alignment-color" => alignment_color(monster)
                 })
              |> capitalize_first

    ApathyDrive.Endpoint.broadcast_from! self, "rooms:#{room.id}", "scroll", %{:html => "<p><span class='grey'>#{message}</span></p>"}
  end

  def display_exit_message(%Room{} = room, monster) when is_pid(monster) do
    display_exit_message(%Room{} = room, Monster.value(monster))
  end
  def display_exit_message(%Room{} = room, %Monster{} = monster) do
    display_exit_message(room, monster, Room.random_direction(room))
  end

  def display_exit_message(%Room{} = room, monster, _direction)  when is_pid(monster) do
    display_exit_message(room, Monster.value(monster), Room.random_direction(room))
  end
  def display_exit_message(%Room{} = room, %Monster{exit_message: exit_message, name: name} = monster, direction) do
    message = exit_message
              |> interpolate(%{
                   "name" => name,
                   "direction" => Room.exit_direction(direction),
                   "alignment-color" => alignment_color(monster)
                 })
              |> capitalize_first

    ApathyDrive.Endpoint.broadcast_from! self, "rooms:#{room.id}", "scroll", %{:html => "<p><span class='grey'>#{message}</span></p>"}
  end

  def ac(%Monster{} = monster) do
    effect_bonus(monster, "ac")
  end

  def local_hated_targets(%Monster{hate: hate} = monster) do
    monster
    |> Room.monsters
    |> Enum.reduce(%{}, fn(potential_target, targets) ->
         threat = HashDict.get(hate, potential_target, 0)
         if threat > 0 do
           Map.put(targets, threat, potential_target)
         else
           targets
         end
       end)
  end

  def global_hated_targets(%Monster{hate: hate}) do
    hate
    |> HashDict.keys
    |> Enum.reduce(%{}, fn(potential_target, targets) ->
         threat = HashDict.get(hate, potential_target, 0)
         if threat > 0 do
           Map.put(targets, threat, potential_target)
         else
           targets
         end
       end)
  end

  def aggro_target(%Monster{} = monster) do
    targets = local_hated_targets(monster)

    top_threat = targets
                 |> Map.keys
                 |> top_threat

    Map.get(targets, top_threat)
  end

  def most_hated_target(%Monster{} = monster) do
    targets = global_hated_targets(monster)

    top_threat = targets
                 |> Map.keys
                 |> top_threat

    Map.get(targets, top_threat)
  end

  def top_threat([]),      do: nil
  def top_threat(targets), do: Enum.max(targets)

  def protection(%Monster{} = monster, damage_type) do
    resistance = monster
                 |> resistance(damage_type)
                 |> resistance

    ac = monster
         |> ac
         |> resistance

    elemental_resistance = effect_bonus(monster, "#{damage_type} resistance")
    elemental_resistance = min(elemental_resistance, 100) / 100.0

    1 - ((1 - resistance_reduction(resistance)) * (1 - resistance_reduction(ac)) * (1 - elemental_resistance))
  end

  def resistance_type("stone"),     do: "physical resistance"
  def resistance_type("normal"),    do: "physical resistance"
  def resistance_type("ice"),       do: "magical resistance"
  def resistance_type("fire"),      do: "magical resistance"
  def resistance_type("lightning"), do: "magical resistance"
  def resistance_type("water"),     do: "magical resistance"
  def resistance_type("poison"),    do: "magical resistance"

  def resistance(%Monster{} = monster, damage_type) do
    type = resistance_type(damage_type)
    effect_bonus(monster, type)
  end

  def resistance(stat) do
    stat = max(0, stat)
    trunc(stat * (0.5 + (stat / 100)))
  end

  def resistance_reduction(resistance) do
    resistance = max(0, resistance)
    resistance / (250 + resistance)
  end

  def reduce_damage(%Monster{} = monster, damage, damage_type) do
    (damage * (1 - protection(monster, damage_type)))
    |> round
  end

  def get_hp_regen(%Monster{hp_regen: regen} = monster) do
    modifier = 1 + effect_bonus(monster, "hp_regen") / 100

    trunc(regen * modifier)
  end

  def get_mana_regen(%Monster{mana_regen: regen} = monster) do
    modifier = 1 + effect_bonus(monster, "mana_regen") / 100

    trunc(regen * modifier)
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

  def handle_call(:value, _from, monster) do
    {:reply, monster, monster}
  end

  def handle_call({:execute_command, command, arguments}, _from, monster) do
    try do
      case ApathyDrive.Command.execute(monster, command, arguments) do
        %Monster{} = monster ->
          {:reply, monster, monster}
        %Spirit{} = spirit ->
          monster = monster
                    |> Map.put(:spirit, nil)
                    |> set_abilities
                    |> save

          {:reply, spirit, monster}
      end
    catch
      kind, error ->
        Monster.send_scroll(monster, "<p><span class='red'>Something went wrong.</span></p>")
        IO.puts Exception.format(kind, error)
        {:reply, monster, monster}
    end
  end

  def handle_call({:possess, %Spirit{level: spirit_level} = spirit},
                                _from,
                                %Monster{level: monster_level, spirit: nil} = monster)
                                when spirit_level < monster_level do
    Spirit.send_scroll(spirit, "<p>You must be at least level #{monster_level} to possess #{monster.name}.</p>")
    {:reply, spirit, monster}
  end

  def handle_call({:possess, %Spirit{} = spirit}, _from, %Monster{spirit: nil} = monster) do
    send(spirit.pid, :go_away)

    spirit = Map.put(spirit, :pid, nil)

    monster = monster
              |> Map.put(:spirit, spirit)
              |> set_abilities
              |> send_scroll("<p>You possess #{monster.name}.")
              |> Monster.save

    ApathyDrive.WhoList.log_on(monster)

    PubSub.subscribe(self, "spirits:online")
    PubSub.subscribe(self, "spirits:hints")
    PubSub.subscribe(self, "spirits:#{spirit.faction}")
    PubSub.subscribe(self, "chat:gossip")
    PubSub.subscribe(self, "chat:#{spirit.faction}")

    PubSub.unsubscribe(self, "rooms:#{monster.room_id}:monsters:#{monster.alignment}")
    PubSub.subscribe(self, "rooms:#{monster.room_id}:monsters:#{monster_alignment(monster)}")

    Systems.Prompt.update(monster)

    {:reply, monster, monster}
  end

  def handle_call({:possess, %Spirit{} = spirit}, _from, %Monster{spirit: _spirit} = monster) do
    Spirit.send_scroll(spirit, "<p>#{capitalize_first(monster.name)} is already possessed.</p>")
    {:reply, spirit, monster}
  end

  def handle_info({:greet, %{greeter: %Monster{pid: greeter_pid},
                             greeted: %Monster{pid: _greeted_pid} = greeted}},
                             %Monster{pid: monster_pid} = monster)
                             when greeter_pid == monster_pid do
    send_scroll(monster, "<p>#{greeted.greeting}</p>")
    {:noreply, monster}
  end

  def handle_info({:greet, %{greeter: %Monster{pid: _greeter_pid} = greeter,
                             greeted: %Monster{pid: greeted_pid}}},
                             %Monster{pid: monster_pid} = monster)
                             when greeted_pid == monster_pid do
    send_scroll(monster, "<p><span class='dark-green'>#{greeter.name |> capitalize_first} greets you.</span></p>")
    {:noreply, monster}
  end

  def handle_info({:greet, %{greeter: greeter, greeted: greeted}}, monster) do
    send_scroll(monster, "<p><span class='dark-green'>#{greeter.name |> capitalize_first} greets #{greeted.name}.</span></p>")
    {:noreply, monster}
  end

  def handle_info({:door_bashed_open, %{basher: %Monster{pid: basher_pid},
                                        type: type}},
                                        %Monster{pid: monster_pid} = monster)
                                        when basher_pid == monster_pid do

    send_scroll(monster, "<p>You bashed the #{type} open.</p>")
    {:noreply, monster}
  end

  def handle_info({:door_bashed_open, %{basher: %Monster{} = basher,
                                        direction: direction,
                                        type: type}},
                                        %Monster{} = monster) do

    send_scroll(monster, "<p>You see #{basher.name} bash open the #{type} #{ApathyDrive.Exit.direction_description(direction)}.</p>")
    {:noreply, monster}
  end

  def handle_info({:mirror_bash, room_exit}, monster) do
    send_scroll(monster, "<p>The #{String.downcase(room_exit["kind"])} #{ApathyDrive.Exit.direction_description(room_exit["direction"])} just flew open!</p>")
    {:noreply, monster}
  end

  def handle_info({:door_bash_failed, %{basher: %Monster{pid: basher_pid}}},
                                        %Monster{pid: monster_pid} = monster)
                                        when basher_pid == monster_pid do

    send_scroll(monster, "<p>Your attempts to bash through fail!</p>")
    {:noreply, monster}
  end

  def handle_info({:door_bash_failed, %{basher: %Monster{} = basher,
                                        direction: direction,
                                        type: type}},
                                        monster) do

    send_scroll(monster, "<p>You see #{basher.name} attempt to bash open the #{type} #{ApathyDrive.Exit.direction_description(direction)}.</p>")
    {:noreply, monster}
  end

  def handle_info({:mirror_bash_failed, room_exit}, monster) do
    send_scroll(monster, "<p>The #{String.downcase(room_exit["kind"])} #{ApathyDrive.Exit.direction_description(room_exit["direction"])} shudders from an impact, but it holds!</p>")
    {:noreply, monster}
  end

  def handle_info({:door_opened, %{opener: %Monster{pid: opener_pid},
                                   type: type}},
                                   %Monster{pid: monster_pid} = monster)
                                   when opener_pid == monster_pid do

    send_scroll(monster, "<p>The #{type} is now open.</p>")
    {:noreply, monster}
  end

  def handle_info({:door_opened, %{opener: %Monster{} = opener,
                                   direction: direction,
                                   type: type}},
                                   %Monster{} = monster) do

    send_scroll(monster, "<p>You see #{opener.name} open the #{type} #{ApathyDrive.Exit.direction_description(direction)}.</p>")
    {:noreply, monster}
  end

  def handle_info({:mirror_open, room_exit}, monster) do
    send_scroll(monster, "<p>The #{String.downcase(room_exit["kind"])} #{ApathyDrive.Exit.direction_description(room_exit["direction"])} just opened.</p>")
    {:noreply, monster}
  end

  def handle_info({:door_closed, %{closer: %Monster{pid: closer_pid},
                                   type: type}},
                                   %Monster{pid: monster_pid} = monster)
                                   when closer_pid == monster_pid do

    send_scroll(monster, "<p>The #{type} is now closed.</p>")
    {:noreply, monster}
  end

  def handle_info({:door_closed, %{closer: %Monster{} = closer,
                                   direction: direction,
                                   type: type}},
                                   %Monster{} = monster) do

    send_scroll(monster, "<p>You see #{closer.name} close the #{type} #{ApathyDrive.Exit.direction_description(direction)}.</p>")
    {:noreply, monster}
  end

  def handle_info({:mirror_close, room_exit}, monster) do
    send_scroll(monster, "<p>The #{String.downcase(room_exit["kind"])} #{ApathyDrive.Exit.direction_description(room_exit["direction"])} just closed.</p>")
    {:noreply, monster}
  end

  def handle_info({:door_locked, %{locker: %Monster{pid: locker_pid},
                                   type: type}},
                                   %Monster{pid: monster_pid} = monster)
                                   when locker_pid == monster_pid do

    send_scroll(monster, "<p>The #{type} is now locked.</p>")
    {:noreply, monster}
  end

  def handle_info({:door_locked, %{locker: %Monster{} = locker,
                                   direction: direction,
                                   type: type}},
                                   %Monster{} = monster) do

    send_scroll(monster, "<p>You see #{locker.name} lock the #{type} #{ApathyDrive.Exit.direction_description(direction)}.</p>")
    {:noreply, monster}
  end

  def handle_info({:mirror_lock, room_exit}, monster) do
    send_scroll(monster, "<p>The #{String.downcase(room_exit["kind"])} #{ApathyDrive.Exit.direction_description(room_exit["direction"])} just locked!</p>")
    {:noreply, monster}
  end

  def handle_info(:regen, %Monster{hp: hp, max_hp: max_hp, mana: mana, max_mana: max_mana} = monster) do
    poison = effect_bonus(monster, "poison")

    if poison > 0 do
      message = monster.effects
                |> Map.values
                |> Enum.find_value(fn(effect) ->
                     Map.has_key?(effect, "poison") && effect["effect_message"]
                   end)

      monster = monster
                |> send_scroll("<p><span class='red'>#{message}</span></p>")
                |> Map.put(:hp,   min(  hp + get_hp_regen(monster) - poison, max_hp))
                |> Map.put(:mana, min(mana + get_mana_regen(monster), max_mana))
                |> Systems.Prompt.update

      if monster.hp < 1, do: Systems.Death.kill(monster)

      {:noreply, monster}
    else
      monster = monster
                |> Map.put(:hp,   min(  hp + get_hp_regen(monster), max_hp))
                |> Map.put(:mana, min(mana + get_mana_regen(monster), max_mana))
                |> Systems.Prompt.update

      if monster.hp < 1, do: Systems.Death.kill(monster)

      {:noreply, monster}
    end

  end

  def handle_info({:timeout, _ref, {name, time, function}}, %Monster{timers: timers} = monster) do
    jitter = trunc(time / 2) + :random.uniform(time)

    new_ref = :erlang.start_timer(jitter, self, {name, time, function})

    timers = Map.put(timers, name, new_ref)

    TimerManager.execute_function(function)

    {:noreply, Map.put(monster, :timers, timers)}
  end

  def handle_info({:timeout, _ref, {name, function}}, %Monster{timers: timers} = monster) do
    TimerManager.execute_function(function)

    timers = Map.delete(timers, name)

    {:noreply, Map.put(monster, :timers, timers)}
  end

  def handle_info({:remove_effect, key}, room) do
    room = Systems.Effect.remove(room, key)
    {:noreply, room}
  end

  def handle_info({:apply_ability, %Ability{} = ability, %Monster{} = ability_user}, monster) do
    if Ability.affects_target?(monster, ability) do
      monster = monster
                |> Ability.apply_ability(ability, ability_user)
                |> Systems.Prompt.update

      if monster.hp < 1, do: Systems.Death.kill(monster)
    else
      message = "#{monster.name} is not affected by that ability." |> capitalize_first
      Monster.send_scroll(ability_user, "<p><span class='dark-cyan'>#{message}</span></p>")
    end

    {:noreply, monster}
  end

  def handle_info({:cast_message, messages: messages,
                                  user: %Monster{pid: user_pid},
                                  target: %Monster{}},
                  %Monster{pid: pid} = monster)
                  when pid == user_pid do

    if messages["user"] do
      send_scroll(monster, messages["user"])
    end

    {:noreply, monster}
  end

  def handle_info({:cast_message, messages: messages,
                                  user: %Monster{},
                                  target: %Monster{pid: target_pid}},
                  %Monster{pid: pid} = monster)
                  when pid == target_pid do

    if messages["target"] do
      send_scroll(monster, messages["target"])
    end

    {:noreply, monster}
  end

  def handle_info({:cast_message, messages: messages,
                                  user: %Monster{},
                                  target: %Monster{}},
                  %Monster{} = monster) do

    if messages["spectator"] do
      send_scroll(monster, messages["spectator"])
    end

    {:noreply, monster}
  end

  def handle_info({:monster_dodged, messages: messages,
                                    user: %Monster{pid: user_pid} = user,
                                    target: %Monster{} = target},
                  %Monster{pid: pid} = monster)
                  when pid == user_pid do

    message = interpolate(messages["user"], %{"user" => user, "target" => target})
    send_scroll(monster, "<p><span class='dark-cyan'>#{message}</span></p>")

    {:noreply, monster}
  end

  def handle_info({:monster_dodged, messages: messages,
                                    user: %Monster{} = user,
                                    target: %Monster{pid: target_pid} = target},
                  %Monster{pid: pid} = monster)
                  when pid == target_pid do

    message = interpolate(messages["target"], %{"user" => user, "target" => target})
    send_scroll(monster, "<p><span class='dark-cyan'>#{message}</span></p>")

    {:noreply, monster}
  end

  def handle_info({:monster_dodged, messages: messages,
                                    user: %Monster{} = user,
                                    target: %Monster{} = target},
                  %Monster{} = monster) do

    message = interpolate(messages["spectator"], %{"user" => user, "target" => target})
    send_scroll(monster, "<p><span class='dark-cyan'>#{message}</span></p>")

    {:noreply, monster}
  end

  def handle_info({:monster_died, monster: %Monster{}, reward: _exp}, %Monster{spirit: nil} = monster) do
    {:noreply, monster}
  end
  def handle_info({:monster_died, monster: %Monster{} = deceased, reward: exp}, %Monster{spirit: %Spirit{} = spirit} = monster) do
    message = deceased.death_message
              |> interpolate(%{"name" => deceased.name})
              |> capitalize_first

    send_scroll(monster, "<p>#{message}</p>")

    new_spirit = Spirit.add_experience(spirit, exp)

    if new_spirit.level > spirit.level do
      monster = monster
                |> Map.put(:spirit, new_spirit)
                |> set_abilities

      {:noreply, monster}
    else
      monster = monster
                |> Map.put(:spirit, new_spirit)

      {:noreply, monster}
    end
  end

  def handle_info({:lair_control_reward, count}, %Monster{spirit: %Spirit{} = spirit} = monster) do
    send_scroll(monster, "<p>You control #{count} lairs.</p>")

    spirit = Spirit.add_experience(spirit, count * spirit.level)

    {:noreply, Map.put(monster, :spirit, spirit)}
  end

  def handle_info({:lair_control_victory_reward, exp}, %Monster{spirit: %Spirit{} = spirit} = monster) do
    send_scroll(monster, "<p>Your faction is in the lead!</p>")

    spirit = Spirit.add_experience(spirit, exp)

    {:noreply, Map.put(monster, :spirit, spirit)}
  end

  def handle_info({:execute_room_ability, ability}, monster) do
    ability = Map.put(ability, :global_cooldown, nil)

    {:noreply, Ability.execute(monster, ability, [self])}
  end

  def handle_info({:execute_ability, ability}, monster) do
    try do
      {:noreply, Ability.execute(monster, ability, [self])}
    catch
      kind, error ->
        Monster.send_scroll(monster, "<p><span class='red'>Something went wrong.</span></p>")
        IO.puts Exception.format(kind, error)
        {:noreply, monster}
    end
  end

  def handle_info({:execute_ability, ability, targets}, monster) do
    try do
      {:noreply, Ability.execute(monster, ability, targets)}
    catch
      kind, error ->
        Monster.send_scroll(monster, "<p><span class='red'>Something went wrong.</span></p>")
        IO.puts Exception.format(kind, error)
        {:noreply, monster}
    end
  end

  def handle_info(:apply_periodic_effects, monster) do

    # periodic damage
    monster.effects
    |> Map.values
    |> Enum.filter(&(Map.has_key?(&1, "damage")))
    |> Enum.each(fn(%{"damage" => damage, "effect_message" => message, "damage_type" => damage_type}) ->
         ability = %Ability{kind: "attack", global_cooldown: nil, flags: [], properties: %{"instant_effects" => %{"damage" => damage}, "damage_type" => damage_type, "cast_message" => %{"user" => message}}}

         send(self, {:apply_ability, ability, monster})
       end)

    # periodic heal
    monster.effects
    |> Map.values
    |> Enum.filter(&(Map.has_key?(&1, "heal")))
    |> Enum.each(fn(%{"heal" => heal}) ->
        ability = %Ability{kind: "heal", global_cooldown: nil, flags: [], properties: %{"instant_effects" => %{"heal" => heal}}}

        send(self, {:apply_ability, ability, monster})
      end)

    # periodic heal_mana
    monster.effects
    |> Map.values
    |> Enum.filter(&(Map.has_key?(&1, "heal_mana")))
    |> Enum.each(fn(%{"heal_mana" => heal}) ->
        ability = %Ability{kind: "heal", global_cooldown: nil, flags: [], properties: %{"instant_effects" => %{"heal_mana" => heal}}}

        send(self, {:apply_ability, ability, monster})
      end)

    {:noreply, monster}
  end

  def handle_info(:think, monster) do
    monster = ApathyDrive.AI.think(monster)

    {:noreply, monster}
  end

  def handle_info(:calm_down, %Monster{hate: hate} = monster) do
    monsters_in_room = ApathyDrive.PubSub.subscribers("rooms:#{monster.room_id}:monsters")
                       |> Enum.into(HashSet.new)

    hate = hate
           |> HashDict.keys
           |> Enum.into(HashSet.new)
           |> HashSet.difference(monsters_in_room)
           |> Enum.reduce(hate, fn(enemy, new_hate) ->
                current = HashDict.fetch!(new_hate, enemy)
                if current > 10 do
                  HashDict.put(new_hate, enemy, current - 10)
                else
                  HashDict.delete(new_hate, enemy)
                end
              end)

    {:noreply, put_in(monster.hate, hate)}
  end

  def handle_info(:set_abilities, monster) do
    {:noreply, set_abilities(monster) }
  end

  def handle_info({:socket_broadcast, message}, monster) do
    Monster.send_scroll(monster, message.payload.html)

    {:noreply, monster}
  end

  def handle_info({:monster_entered, intruder, intruder_alignment}, monster) do
    monster = Systems.Aggression.react(%{monster: monster, alignment: monster_alignment(monster)}, %{intruder: intruder, alignment: intruder_alignment})

    2000
    |> :random.uniform
    |> :erlang.send_after(self, :think)

    {:noreply, monster}
  end

  def handle_info({:monster_left, coward, direction}, monster) do
    if Monster.most_hated_target(monster) == coward and :random.uniform(100) < monster.chance_to_follow do
      monster = ApathyDrive.Exit.move(monster, direction)
      {:noreply, monster}
    else
      {:noreply, monster}
    end
  end

  def handle_info({:gossip, name, message}, monster) do
    Monster.send_scroll(monster, "<p>[<span class='dark-magenta'>Gossip</span> : #{name}] #{message}</p>")
    {:noreply, monster}
  end

  def handle_info({:good, name, message}, monster) do
    Monster.send_scroll(monster, "<p>[<span class='white'>Good</span> : #{name}] #{message}</p>")
    {:noreply, monster}
  end

  def handle_info({:neutral, name, message}, monster) do
    Monster.send_scroll(monster, "<p>[<span class='dark-cyan'>Neutral</span> : #{name}] #{message}</p>")
    {:noreply, monster}
  end

  def handle_info({:evil, name, message}, monster) do
    Monster.send_scroll(monster, "<p>[<span class='magenta'>Evil</span> : #{name}] #{message}</p>")
    {:noreply, monster}
  end

  def handle_info({:scroll, html}, monster) do
    {:noreply, send_scroll(monster, html)}
  end

  def handle_info(_message, monster) do
    {:noreply, monster}
  end

end
