defmodule Monster do
  require Logger
  use Ecto.Model
  use GenServer

  import Systems.Text
  alias ApathyDrive.Repo
  alias ApathyDrive.PubSub

  schema "monsters" do
    field :name,                :string
    field :skills,              ApathyDrive.JSONB, default: %{}
    field :limbs,               ApathyDrive.JSONB
    field :experience,          :integer, default: 0
    field :level,               :integer, default: 1
    field :alignment,           :integer
    field :lair_id,             :integer
    field :max_hp,              :integer, virtual: true
    field :hp,                  :integer, virtual: true
    field :mana,                :integer, virtual: true
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
    field :strength,            :integer, virtual: true
    field :agility,             :integer, virtual: true
    field :intelligence,        :integer, virtual: true
    field :health,              :integer, virtual: true
    field :hit_verbs,           :any,     virtual: true
    field :chance_to_follow,    :integer, virtual: true
    field :damage,              :any,     virtual: true
    field :possession_level,    :integer, virtual: true
    field :questions,           :any,     virtual: true
    field :pid,                 :any,     virtual: true
    field :keywords,            {:array, :string}, virtual: true
    field :flags,               {:array, :string}, virtual: true
    field :hate,                :any, virtual: true, default: HashDict.new
    field :inventory,           :any, virtual: true, default: []
    field :equipment,           :any, virtual: true, default: %{}

    timestamps

    belongs_to :room, Room
    belongs_to :monster_template, MonsterTemplate
  end

  def init(%Monster{} = monster) do
    :random.seed(:os.timestamp)

    if monster.room_id do
      PubSub.subscribe(self, "rooms:#{monster.room_id}")
      PubSub.subscribe(self, "rooms:#{monster.room_id}:monsters")
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
              |> TimerManager.call_every({:monster_ai,    5_000, fn -> send(self, :think) end})
              |> TimerManager.call_every({:monster_regen, 5_000, fn -> send(self, :regen) end})
              |> TimerManager.call_every({:calm_down,    10_000, fn -> send(self, :calm_down) end})

    {:ok, monster}
  end

  def set_abilities(%Monster{monster_template_id: nil} = monster) do
    monster
  end
  def set_abilities(%Monster{} = monster) do
    abilities = monster_template_abilities(monster)

    abilities = abilities ++
                abilities_from_equipment(monster, abilities) ++
                abilities_from_skills(monster)

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
  end

  def attack_abilities(%Monster{abilities: abilities} = monster) do
    abilities
    |> Enum.filter(&(&1.kind == "attack"))
    |> Ability.useable(monster)
  end

  def monster_template_abilities(%Monster{} = monster) do
    mt = monster.monster_template_id
         |> MonsterTemplate.find
         |> MonsterTemplate.value

    mt.abilities
    |> Enum.map(&(Repo.get(Ability, &1)))
  end

  def abilities_from_skills(monster) do
    base_skills = base_skills(monster)

    Ability.trainable
    |> Enum.filter(fn(%Ability{} = ability) ->
         ability.required_skills
         |> Map.keys
         |> Enum.all?(fn(required_skill) ->
              monster_skill  = Map.get(base_skills, required_skill, 0)
              required_skill = Map.get(ability.required_skills, required_skill, 0)

              monster_skill >= required_skill
            end)
       end)
  end

  def abilities_from_equipment(%Monster{} = monster, abilities) do
    monster.equipment
    |> Map.values
    |> Enum.find(&(Enum.member?(["Weapon Hand", "Two Handed"], &1.worn_on)))
    |> abilities_from_weapon(abilities)
  end

  def abilities_from_weapon(nil, []) do
    [
      %Ability{
        name:    "attack",
        command: "a",
        kind:    "attack",
        required_skills: %{"melee" => 0},
        global_cooldown: 4,
        flags: [],
        properties: %{
          "dodgeable" => true,
          "accuracy_skill" => "melee",
          "dodge_message" => %{
            "target" => "You dodge {{user}}'s attack!",
            "user" => "{{Target}} dodges your attack!",
            "spectator" => "{{Target}} dodges {{user}}'s attack!"
          },
          "instant_effects" => %{
            "damage" => %{
              "scaling" => %{
                "melee" => %{
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

  def abilities_from_weapon(nil, abilities) do
    attacks = abilities
              |> Enum.filter(&(&1.kind == "attack"))

    if Enum.any?(attacks) do
      []
    else
      abilities_from_weapon(nil, [])
    end
  end

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

  def execute_command(monster, command, arguments) do
    GenServer.cast(monster, {:execute_command, command, arguments})
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
                  |> Enum.into(Keyword.new)

        monster = struct(Monster, monster)

        monster = monster
                  |> Map.put(:hp, monster.max_hp)
                  |> Map.put(:mana, Monster.max_mana(monster))
                  |> Map.put(:keywords, String.split(monster.name))

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

    PubSub.subscribe(self, "rooms:#{room_id}")
    PubSub.subscribe(self, "rooms:#{room_id}:monsters")
    monster
    |> Map.put(:room_id, room_id)
    |> Systems.Effect.add(%{"cooldown" => :ai_movement}, 30)
  end

  def effect_description(%Monster{effects: effects} = monster) do
    effects
    |> Map.values
    |> Enum.find(fn(effect) ->
         Map.has_key?(effect, "description")
       end)
    |> effect_description
  end

  def effect_description(nil), do: nil
  def effect_description(%{"description" => description}), do: description

  def max_mana(%Monster{} = monster) do
    intelligence = modified_stat(monster, "intelligence")

    x = trunc(intelligence * (0.5 + (intelligence / 100)))
    y = x / (125 + x)
    trunc((x / 10) + (x * (1 - y)))
  end

  def pre_effect_bonus_stat(%Monster{} = monster, stat_name) do
    Map.get(monster, String.to_atom(stat_name), 0) +
    stat_skill_bonus(monster, stat_name)
  end

  def modified_stat(%Monster{} = monster, stat_name) do
    pre_effect_bonus_stat(monster, stat_name) +
    effect_bonus(monster, stat_name)
  end

  def stat_skill_bonus(%Monster{} = monster, stat_name) do
    Skill.with_modifier(stat_name, trained_skills(monster))
    |> Enum.reduce(0, fn(skill, total_stat_modification) ->
         base = skill_from_training(monster, skill.name)
         percentage = Skill.modifier_percentage(skill, stat_name)
         total_stat_modification + base * percentage
       end)
    |> trunc
  end

  def trained_skills(%Monster{skills: skills}) do
    skills
    |> Map.keys
    |> Enum.filter(fn(skill_name) ->
         trained = skills
                   |> Map.get(skill_name)
                   |> Map.get("trained", 0)
         trained > 0
       end)
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

  def base_skill(%Monster{skills: skills} = monster, skill_name) do
    base = skills
           |> Map.get(skill_name, %{})
           |> Map.get("base", 0)

    base + skill_from_training(monster, skill_name)
  end

  def modified_skill(%Monster{} = monster, skill_name) do
    skill = Skill.find(skill_name)

    base = base_skill(monster, skill_name)

    modified = if base > 0 do
      total = ["strength", "intelligence", "agility", "health"]
              |> Enum.reduce(0, fn(stat, total) ->
                   total + modified_stat(monster, stat) * Map.get(skill, String.to_atom(stat), 0)
                 end)

      average = total / Skill.modifier_total(skill)

      light_level = monster
                    |> find_room
                    |> Room.light_level(monster)

      round(base * (1 + average * 0.005))
      |> Skill.modify_for_room_light(light_level)
    else
      0
    end
    (modified + effect_bonus(monster, skill_name))
  end

  def skill_from_training(%Monster{skills: skills}, skill_name) do
    skill = Skill.find(skill_name)

    power_spent = skills
                  |> Map.get(skill_name, %{})
                  |> Map.get("trained", 0)

    modifier = skill.cost

    skill_from_training(modifier, power_spent)
  end
  def skill_from_training(modifier, power_spent) do
    skill_from_training(0, modifier, cost(modifier, 0), power_spent)
  end
  def skill_from_training(rating, modifier, cost, power) when power >= cost do
    new_rating = rating + 1
    new_cost = cost(modifier, new_rating)
    skill_from_training(new_rating, modifier, new_cost, power - cost)
  end
  def skill_from_training(rating, _modifier, cost, power) when power < cost do
    rating
  end

  def cost(modifier, rating) do
    [rating * modifier * 1.0 |> Float.ceil |> trunc, 1] |> Enum.max
  end

  def send_scroll(%Monster{id: id} = monster, html) do
    ApathyDrive.Endpoint.broadcast! "monsters:#{id}", "scroll", %{:html => html}
    monster
  end

  def send_disable(%Monster{id: id} = monster, elem) do
    ApathyDrive.Endpoint.broadcast! "monsters:#{id}", "disable", %{:html => elem}
    monster
  end

  def send_focus(%Monster{id: id} = monster, elem) do
    ApathyDrive.Endpoint.broadcast! "monsters:#{id}", "focus", %{:html => elem}
    monster
  end

  def send_up(%Monster{id: id} = monster) do
    ApathyDrive.Endpoint.broadcast! "monsters:#{id}", "up", %{}
    monster
  end

  def send_update_prompt(%Monster{id: id} = monster, html) do
    ApathyDrive.Endpoint.broadcast! "monsters:#{id}", "update prompt", %{:html => html}
    monster
  end

  def look_name(%Monster{} = monster) do
    cond do
      evil?(monster) ->
        "<span class='magenta'>#{monster.name}</span>"
      good?(monster) ->
        "<span class='grey'>#{monster.name}</span>"
      neutral?(monster) ->
        "<span class='dark-cyan'>#{monster.name}</span>"
    end
  end

  def good?(%Monster{alignment: alignment}) when alignment < -50, do: true
  def good?(%Monster{}), do: false
  def evil?(%Monster{alignment: alignment}) when alignment > 50,  do: true
  def evil?(%Monster{}), do: false
  def neutral?(%Monster{} = monster), do: !good?(monster) and !evil?(monster)

  def get_alignment(%Monster{} = monster) do
    cond do
      good?(monster) ->
        "good"
      evil?(monster) ->
        "evil"
      true ->
        "neutral"
    end
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
  def display_enter_message(%Room{} = room, %Monster{enter_message: enter_message, name: name}, direction) do
    message = enter_message
              |> interpolate(%{
                   "name" => name,
                   "direction" => Room.enter_direction(direction)
                 })
              |> capitalize_first

    ApathyDrive.Endpoint.broadcast_from! self, "rooms:#{room.id}", "scroll", %{:html => "<p><span class='dark-green'>#{message}</span></p>"}
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
  def display_exit_message(%Room{} = room, %Monster{exit_message: exit_message, name: name}, direction) do
    message = exit_message
              |> interpolate(%{
                   "name" => name,
                   "direction" => Room.exit_direction(direction)
                 })
              |> capitalize_first

    ApathyDrive.Endpoint.broadcast_from! self, "rooms:#{room.id}", "scroll", %{:html => "<p><span class='dark-green'>#{message}</span></p>"}
  end

  defp regen_rate(seed) when is_integer(seed) do
    regen_rate(trunc(seed / 2), 0)
  end

  defp regen_rate(seed, rate) when seed > 0 do
    regen_rate(seed - 1, rate + ((seed - 1) / 100))
  end

  defp regen_rate(0, rate) do
    trunc(rate)
  end

  def hp_regen_per_tick(%Monster{} = monster) do
    regen_rate(modified_stat(monster, "health"))
    |> max(1)
  end

  def mana_regen_per_tick(%Monster{} = monster) do
    regen_rate(modified_stat(monster, "intelligence"))
    |> max(1)
  end

  def ac(%Monster{} = monster) do
    ac_from_equipment(monster) + effect_bonus(monster, "ac")
  end

  def ac_from_equipment(%Monster{} = monster) do
    monster.equipment
    |> Map.values
    |> Enum.map(&(&1.ac || 0))
    |> Enum.sum
  end

  def local_hated_targets(%Monster{hate: hate, pid: pid} = monster) do
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

  def global_hated_targets(%Monster{hate: hate, pid: pid} = monster) do
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
    resistance = 0 #resistance(entity, CritTables.damage_types[to_string(damage_type)])
    ac = monster
         |> ac
         |> resistance

    elemental_resistance = effect_bonus(monster, "#{damage_type} resistance")
    elemental_resistance = min(elemental_resistance, 100) / 100.0

    1 - ((1 - resistance_reduction(resistance)) * (1 - resistance_reduction(ac)) * (1 -elemental_resistance))
  end

  def resistance(stat) do
    trunc(stat * (0.5 + (stat / 100)))
  end

  def resistance_reduction(resistance) do
    resistance / (250 + resistance)
  end

  def reduce_damage(%Monster{} = monster, damage, damage_type) do
    (damage * (1 - protection(monster, damage_type)))
    |> round
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

  def handle_cast({:execute_command, command, arguments}, monster) do
    monster = ApathyDrive.Command.execute(monster, command, arguments)
    {:noreply, monster}
  end

  def handle_info({:greet, %{greeter: %Monster{pid: greeter_pid},
                             greeted: %Monster{pid: _greeted_pid} = greeted}},
                             %Monster{pid: monster_pid} = monster)
                             when greeter_pid == monster_pid do
    send_scroll(monster, "<p><span class='dark-green'>#{greeted.greeting}</span></p>")
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
    send_scroll(monster, "<p><span class='dark-green'>#{greeter.name |> capitalize_first} greets #{greeted}.</span></p>")
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

  def handle_info({:door_picked, %{picker: %Monster{pid: picker_pid},
                                   type: type}},
                                   %Monster{pid: monster_pid} = monster)
                                   when picker_pid == monster_pid do

    send_scroll(monster, "<p>You successfully unlocked the #{type}.</p>")
    {:noreply, monster}
  end

  def handle_info({:door_picked, %{basher: %Monster{} = picker,
                                   direction: direction,
                                   type: type}},
                                   %Monster{} = monster) do

    send_scroll(monster, "<p>You see #{picker.name} pick the lock on the #{type} #{ApathyDrive.Exit.direction_description(direction)}.</p>")
    {:noreply, monster}
  end

  def handle_info({:mirror_pick, room_exit}, monster) do
    send_scroll(monster, "<p>The #{String.downcase(room_exit["kind"])} #{ApathyDrive.Exit.direction_description(room_exit["direction"])} unlocks with a click.</p>")
    {:noreply, monster}
  end

  def handle_info({:door_pick_failed, %{picker: %Monster{pid: picker_pid}}},
                                        %Monster{pid: monster_pid} = monster)
                                        when picker_pid == monster_pid do

    send_scroll(monster, "<p>Your skill fails you this time.</p>")
    {:noreply, monster}
  end

  def handle_info({:door_pick_failed, %{picker: %Monster{} = picker,
                                        direction: direction,
                                        type: type}},
                                        monster) do

    send_scroll(monster, "<p>You see #{picker.name} attempt to pick the lock on the #{type} #{ApathyDrive.Exit.direction_description(direction)}.</p>")
    {:noreply, monster}
  end

  def handle_info({:mirror_pick_failed, room_exit}, monster) do
    send_scroll(monster, "<p>You hear a scratching sound in the lock on the #{String.downcase(room_exit["kind"])} #{ApathyDrive.Exit.direction_description(room_exit["direction"])}.</p>")
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

  def handle_info(:regen, %Monster{hp: hp, max_hp: max_hp, mana: mana} = monster) do
    max_mana = max_mana(monster)

    monster = monster
              |> Map.put(:hp,   min(  hp + hp_regen_per_tick(monster),   max_hp))
              |> Map.put(:mana, min(mana + mana_regen_per_tick(monster), max_mana))
              |> Systems.Prompt.update

    {:noreply, monster}
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

      if monster.hp < 0, do: Systems.Death.kill(monster)
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

    send_scroll(monster, messages["user"])

    {:noreply, monster}
  end

  def handle_info({:cast_message, messages: messages,
                                  user: %Monster{},
                                  target: %Monster{pid: target_pid}},
                  %Monster{pid: pid} = monster)
                  when pid == target_pid do

    send_scroll(monster, messages["target"])

    {:noreply, monster}
  end

  def handle_info({:cast_message, messages: messages,
                                  user: %Monster{},
                                  target: %Monster{}},
                  %Monster{} = monster) do

    send_scroll(monster, messages["spectator"])

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

  def handle_info({:monster_died, monster: %Monster{} = deceased, reward: exp}, monster) do
    message = deceased.death_message
              |> interpolate(%{"name" => deceased.name})
              |> capitalize_first

    old_power = Systems.Trainer.total_power(monster)

    monster = monster
              |> send_scroll("<p>#{message}</p>")
              |> send_scroll("<p>You gain #{exp} experience.</p>")
              |> Map.put(:experience, monster.experience + exp)

    new_power = Systems.Trainer.total_power(monster)

    power_gain = new_power - old_power

    if power_gain > 0 do
      send_scroll(monster, "<p>You gain #{power_gain} development points.</p>")
    end

    monster = monster
              |> Systems.Level.advance
              |> Monster.save

    PubSub.broadcast!("monsters:#{monster.id}", {:reward_possessor, exp})

    {:noreply, monster}
  end

  def handle_info({:execute_room_ability, ability}, monster) do
    ability = Map.put(ability, :global_cooldown, nil)

    {:noreply, Ability.execute(monster, ability, monster)}
  end

  def handle_info({:execute_ability, ability}, monster) do
    {:noreply, Ability.execute(monster, ability, monster)}
  end

  def handle_info({:execute_ability, ability, target}, monster) do
    {:noreply, Ability.execute(monster, ability, target)}
  end

  def handle_info(:think, monster) do
    Systems.AI.think(monster)

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
    monster = Systems.Aggression.react(%{monster: monster, alignment: Monster.get_alignment(monster)}, %{intruder: intruder, alignment: intruder_alignment})

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

  def handle_info(_message, monster) do
    {:noreply, monster}
  end

end
