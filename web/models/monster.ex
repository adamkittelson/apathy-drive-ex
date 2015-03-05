defmodule Monster do
  require Logger
  use Ecto.Model
  use GenServer
  use Systems.Reload
  import Systems.Text
  alias ApathyDrive.Repo
  alias Phoenix.PubSub

  schema "monsters" do
    field :name,                :string
    field :skills,              ApathyDrive.JSONB, default: %{}
    field :limbs,               ApathyDrive.JSONB
    field :experience,          :integer, default: 0
    field :level,               :integer, default: 1
    field :alignment,           :integer
    field :lair_id,             :integer
    field :hp,                  :integer, virtual: true
    field :mana,                :integer, virtual: true
    field :hunting,             :any,     virtual: true, default: []
    field :combat,              :any,     virtual: true, default: %{"break_at" => 0}
    field :flags,               :any,     virtual: true, default: %{}
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

    timestamps

    belongs_to :room, Room
    belongs_to :monster_template, MonsterTemplate
    has_many   :items, Item
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
              |> set_abilities

    :global.register_name(:"monster_#{monster.id}", self)

    {:ok, monster}
  end

  def set_abilities(%Monster{monster_template_id: nil} = monster) do
    monster
  end
  def set_abilities(%Monster{} = monster) do
    abilities = monster_template_abilities(monster) ++
                abilities_from_skills(monster)
    monster
    |> Map.put(:abilities, abilities)
  end

  def monster_template_abilities(%Monster{} = monster) do
    monster.monster_template.abilities
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

  def execute_command(monster, command, arguments) do
    GenServer.cast(monster, {:execute_command, command, arguments})
  end

  def value(monster) do
    GenServer.call(monster, :value)
  end

  def insert(%Monster{id: nil} = monster) do
    ApathyDrive.Repo.insert(monster)
  end
  def insert(%Monster{} = monster), do: monster

  def save(monster) when is_pid(monster), do: monster |> value |> save
  def save(%Monster{id: id} = monster) when is_integer(id) do
    Repo.update(monster)
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
    case Repo.one from m in Monster, where: m.id == ^id, preload: [:monster_template] do
      %Monster{} = monster ->

        mt = monster.monster_template_id
             |> MonsterTemplate.find
             |> MonsterTemplate.value

        monster = Map.merge(mt, monster, fn(key, mt_val, monster_val) ->
                    monster_val || mt_val
                  end)
                  |> Map.from_struct
                  |> Enum.into(Keyword.new)

        monster = struct(Monster, monster)

        monster = monster
                  |> Map.put(:hp, Monster.max_hp(monster))
                  |> Map.put(:mana, Monster.max_mana(monster))
                  |> Map.put(:keywords, String.split(monster.name))

        monster
        |> item_ids
        |> Enum.each(&Item.find/1)

        {:ok, pid} = Supervisor.start_child(ApathyDrive.Supervisor, {:"monster_#{monster.id}", {GenServer, :start_link, [Monster, monster, []]}, :transient, 5000, :worker, [Monster]})

        pid
      nil ->
        nil
    end
  end

  def item_ids(%Monster{} = monster) do
    Repo.all(from i in assoc(monster, :items), select: i.id)
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
    Map.put(monster, :room_id, room_id)
  end

  def inventory(%Monster{id: id} = monster) do
    PubSub.subscribers("monsters:#{id}:inventory")
    |> Enum.map(&Item.value/1)
  end

  def equipped_items(%Monster{id: id} = monster) do
    PubSub.subscribers("monsters:#{id}:equipped_items")
    |> Enum.map(&Item.value/1)
  end

  def display_inventory(%Monster{id: id} = monster) do
    if Enum.any?(equipment = equipped_items(monster)) do
      Monster.send_scroll(monster, "<p><span class='dark-yellow'>You are equipped with:</span></p><br>")
      Enum.each equipment, fn(item) ->
        send_scroll(monster, "<p><span class='dark-green'>#{String.ljust(item.name, 23)}</span><span class='dark-cyan'>(#{item.worn_on})</span></p>")
      end
      send_scroll(monster, "<br>")
    end

    items = inventory(monster) |> Enum.map(&(&1.name))
    if items |> Enum.count > 0 do
      send_scroll(monster, "<p>You are carrying #{Enum.join(items, ", ")}</p>")
    else
      send_scroll(monster, "<p>You are carrying nothing.</p>")
    end
  end

  def equip_item(%Monster{} = monster, %Item{} = item, nil) do
    send(item.pid, :equip)
    monster
  end

  def equip_item(%Monster{} = monster, %Item{}, {skill, req}) do
    Monster.send_scroll(monster, "<p>You need at least #{req} #{skill} skill to equip that.</p>")
  end

  def max_hp(%Monster{} = monster) do
    health   = modified_stat(monster, "health")
    strength = modified_stat(monster, "strength")

    seed = trunc((health * 2 + strength) / 3)

    trunc(seed * (11 + (seed / 10)))
  end

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

  def effect_bonus(%Monster{effects: effects} = monster, name) do
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
                   total + modified_stat(monster, stat) * Map.get(skill, stat, 0)
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
    Phoenix.Channel.broadcast "monsters:#{id}", "scroll", %{:html => html}
    monster
  end

  def send_disable(%Monster{id: id} = monster, elem) do
    Phoenix.Channel.broadcast "monsters:#{id}", "disable", %{:html => elem}
    monster
  end

  def send_focus(%Monster{id: id} = monster, elem) do
    Phoenix.Channel.broadcast "monsters:#{id}", "focus", %{:html => elem}
    monster
  end

  def send_up(%Monster{id: id} = monster) do
    Phoenix.Channel.broadcast "monsters:#{id}", "up", %{}
    monster
  end

  def send_update_prompt(%Monster{id: id} = monster, html) do
    Phoenix.Channel.broadcast "monsters:#{id}", "update prompt", %{:html => html}
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

  def display_enter_message(%Room{} = room, monster) when is_pid(monster) do
    display_enter_message(%Room{} = room, Monster.value(monster))
  end
  def display_enter_message(%Room{} = room, %Monster{} = monster) do
    display_enter_message(room, monster, Room.random_direction(room))
  end

  def display_enter_message(%Room{} = room, monster, direction)  when is_pid(monster) do
    display_enter_message(room, Monster.value(monster), Room.random_direction(room))
  end
  def display_enter_message(%Room{} = room, %Monster{enter_message: enter_message, name: name}, direction) do
    message = enter_message
              |> interpolate(%{
                   "name" => name,
                   "direction" => Room.enter_direction(direction)
                 })
              |> capitalize_first

    Phoenix.Channel.broadcast "rooms:#{room.id}", "scroll", %{:html => "<p><span class='dark-green'>#{message}</span></p>"}
  end

  def display_exit_message(%Room{} = room, monster) when is_pid(monster) do
    display_exit_message(%Room{} = room, Monster.value(monster))
  end
  def display_exit_message(%Room{} = room, %Monster{} = monster) do
    display_exit_message(room, monster, Room.random_direction(room))
  end

  def display_exit_message(%Room{} = room, monster, direction)  when is_pid(monster) do
    display_exit_message(room, Monster.value(monster), Room.random_direction(room))
  end
  def display_exit_message(%Room{} = room, %Monster{exit_message: exit_message, name: name} = monster, direction) do
    message = exit_message
              |> interpolate(%{
                   "name" => name,
                   "direction" => Room.exit_direction(direction)
                 })
              |> capitalize_first

    Phoenix.Channel.broadcast "rooms:#{room.id}", "scroll", %{:html => "<p><span class='dark-green'>#{message}</span></p>"}
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

  def handle_info({:greet, %{greeter: %Monster{pid: greeter_pid} = greeter,
                             greeted: %Monster{pid: _greeted_pid} = greeted}},
                             %Monster{pid: monster_pid} = monster)
                             when greeter_pid == monster_pid do
    send_scroll(monster, "<p><span class='dark-green'>#{greeted.greeting}</span></p>")
    {:noreply, monster}
  end

  def handle_info({:greet, %{greeter: %Monster{pid: _greeter_pid} = greeter,
                             greeted: %Monster{pid: greeted_pid} = greeted}},
                             %Monster{pid: monster_pid} = monster)
                             when greeted_pid == monster_pid do
    send_scroll(monster, "<p><span class='dark-green'>#{greeter.name |> capitalize_first} greets you.</span></p>")
    {:noreply, monster}
  end

  def handle_info({:greet, %{greeter: greeter, greeted: greeted}}, monster) do
    send_scroll(monster, "<p><span class='dark-green'>#{greeter.name |> capitalize_first} greets #{greeted}.</span></p>")
    {:noreply, monster}
  end

  def handle_info({:door_bashed_open, %{basher: %Monster{pid: basher_pid} = basher,
                                        direction: direction,
                                        type: type}},
                                        %Monster{pid: monster_pid} = monster)
                                        when basher_pid == monster_pid do

    send_scroll(monster, "<p>You bashed the #{type} open.</p>")
    {:noreply, monster}
  end

  def handle_info({:door_bashed_open, %{basher: %Monster{pid: basher_pid} = basher,
                                        direction: direction,
                                        type: type}},
                                        %Monster{pid: monster_pid} = monster) do

    send_scroll(monster, "<p>You see #{basher.name} bash open the #{type} #{ApathyDrive.Exit.direction_description(direction)}.</p>")
    {:noreply, monster}
  end

  def handle_info({:mirror_bash, room_exit}, monster) do
    send_scroll(monster, "<p>The #{String.downcase(room_exit["kind"])} #{ApathyDrive.Exit.direction_description(room_exit["direction"])} just flew open!</p>")
    {:noreply, monster}
  end

  def handle_info({:door_bash_failed, %{basher: %Monster{pid: basher_pid} = basher,
                                        direction: direction}},
                                        %Monster{pid: monster_pid} = monster)
                                        when basher_pid == monster_pid do

    send_scroll(monster, "<p>Your attempts to bash through fail!</p>")
    {:noreply, monster}
  end

  def handle_info({:door_bash_failed, %{basher: %Monster{pid: basher_pid} = basher,
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

  def handle_info({:door_opened, %{opener: %Monster{pid: opener_pid} = opener,
                                   direction: direction,
                                   type: type}},
                                   %Monster{pid: monster_pid} = monster)
                                   when opener_pid == monster_pid do

    send_scroll(monster, "<p>The #{type} is now open.</p>")
    {:noreply, monster}
  end

  def handle_info({:door_opened, %{opener: %Monster{pid: basher_pid} = opener,
                                   direction: direction,
                                   type: type}},
                                   %Monster{pid: monster_pid} = monster) do

    send_scroll(monster, "<p>You see #{opener.name} open the #{type} #{ApathyDrive.Exit.direction_description(direction)}.</p>")
    {:noreply, monster}
  end

  def handle_info({:mirror_open, room_exit}, monster) do
    send_scroll(monster, "<p>The #{String.downcase(room_exit["kind"])} #{ApathyDrive.Exit.direction_description(room_exit["direction"])} just opened.</p>")
    {:noreply, monster}
  end

  def handle_info({:door_closed, %{closer: %Monster{pid: closer_pid} = closer,
                                   direction: direction,
                                   type: type}},
                                   %Monster{pid: monster_pid} = monster)
                                   when closer_pid == monster_pid do

    send_scroll(monster, "<p>The #{type} is now closed.</p>")
    {:noreply, monster}
  end

  def handle_info({:door_closed, %{closer: %Monster{pid: closer_pid} = closer,
                                   direction: direction,
                                   type: type}},
                                   %Monster{pid: monster_pid} = monster) do

    send_scroll(monster, "<p>You see #{closer.name} close the #{type} #{ApathyDrive.Exit.direction_description(direction)}.</p>")
    {:noreply, monster}
  end

  def handle_info({:mirror_close, room_exit}, monster) do
    send_scroll(monster, "<p>The #{String.downcase(room_exit["kind"])} #{ApathyDrive.Exit.direction_description(room_exit["direction"])} just closed.</p>")
    {:noreply, monster}
  end

  def handle_info({:door_picked, %{picker: %Monster{pid: picker_pid} = picker,
                                   direction: direction,
                                   type: type}},
                                   %Monster{pid: monster_pid} = monster)
                                   when picker_pid == monster_pid do

    send_scroll(monster, "<p>You successfully unlocked the #{type}.</p>")
    {:noreply, monster}
  end

  def handle_info({:door_picked, %{basher: %Monster{pid: picker_pid} = picker,
                                   direction: direction,
                                   type: type}},
                                   %Monster{pid: monster_pid} = monster) do

    send_scroll(monster, "<p>You see #{picker.name} pick the lock on the #{type} #{ApathyDrive.Exit.direction_description(direction)}.</p>")
    {:noreply, monster}
  end

  def handle_info({:mirror_pick, room_exit}, monster) do
    send_scroll(monster, "<p>The #{String.downcase(room_exit["kind"])} #{ApathyDrive.Exit.direction_description(room_exit["direction"])} unlocks with a click.</p>")
    {:noreply, monster}
  end

  def handle_info({:door_pick_failed, %{picker: %Monster{pid: picker_pid} = picker,
                                        direction: direction}},
                                        %Monster{pid: monster_pid} = monster)
                                        when picker_pid == monster_pid do

    send_scroll(monster, "<p>Your skill fails you this time.</p>")
    {:noreply, monster}
  end

  def handle_info({:door_pick_failed, %{picker: %Monster{pid: picker_pid} = picker,
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

  def handle_info(:regen, %Monster{hp: hp, mana: mana} = monster) do
    max_hp   = max_hp(monster)
    max_mana = max_mana(monster)

    monster = monster
              |> Map.put(:hp,   min(  hp + hp_regen_per_tick(monster),   max_hp))
              |> Map.put(:mana, min(mana + mana_regen_per_tick(monster), max_mana))
              |> Systems.Prompt.update

    {:noreply, monster}
  end

  def handle_info({:timeout, _ref, {name, time, function}}, %Monster{timers: timers} = monster) do
    new_ref = :erlang.start_timer(time, self, {name, time, function})

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
    monster = monster
              |> Ability.apply_ability(ability, ability_user)
              |> Systems.Prompt.update

    if monster.hp < 0, do: Systems.Death.kill(monster)

    {:noreply, monster}
  end

  def handle_info({:cast_message, messages: messages,
                                  user: %Monster{pid: user_pid} = user,
                                  target: %Monster{pid: target_pid} = target},
                  %Monster{pid: pid} = monster)
                  when pid == user_pid do

    send_scroll(monster, messages["user"])

    {:noreply, monster}
  end

  def handle_info({:cast_message, messages: messages,
                                  user: %Monster{pid: user_pid} = user,
                                  target: %Monster{pid: target_pid} = target},
                  %Monster{pid: pid} = monster)
                  when pid == target_pid do

    send_scroll(monster, messages["target"])

    {:noreply, monster}
  end

  def handle_info({:cast_message, messages: messages,
                                  user: %Monster{pid: user_pid} = user,
                                  target: %Monster{pid: target_pid} = target},
                  %Monster{pid: pid} = monster) do

    send_scroll(monster, messages["spectator"])

    {:noreply, monster}
  end

  def handle_info({:item_equipped, %Item{} = item}, monster) do
    send_scroll(monster, "<p>You are now wearing #{item.name}.</p>")
    {:noreply, monster}
  end

  def handle_info({:item_unequipped, %Item{} = item}, monster) do
    send_scroll(monster, "<p>You remove #{item.name}.</p>")
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

    PubSub.broadcast("monsters:#{monster.id}", {:reward_possessor, exp})

    {:noreply, monster}
  end

  def handle_info({:item_destroyed, %Item{room_destruct_message: nil} = item}, monster) do
    Monster.send_scroll(monster, "<p>#{item.destruct_message}</p>")
    {:noreply, monster}
  end

  def handle_info({:item_destroyed, %Item{} = item}, monster) do
    Monster.send_scroll(monster, "<p>#{item.destruct_message}</p>")

    PubSub.broadcast_from(self, "rooms:#{item.room_id}", {:room_item_destroyed, item, monster})

    {:noreply, monster}
  end

  def handle_info({:room_item_destroyed, %Item{} = item, %Monster{} = holder}, monster) do
    Monster.send_scroll(monster, "<p>#{item.room_destruct_message |> interpolate(%{"user" => holder})}</p>")

    {:noreply, monster}
  end

  def handle_info(_message, monster) do
    {:noreply, monster}
  end

end
