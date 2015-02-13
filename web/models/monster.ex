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
    field :monster_template_id, :integer
    field :experience,          :integer, default: 0
    field :level,               :integer, default: 1
    field :alignment,           :decimal
    field :lair_id,             :integer, virtual: true
    field :hp,                  :integer, virtual: true
    field :mana,                :integer, virtual: true
    field :hunting,             :any,     virtual: true, default: []
    field :combat,              :any,     virtual: true, default: %{"break_at" => 0}
    field :flags,               :any,     virtual: true, default: %{}
    field :effects,             :any,     virtual: true, default: %{}
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
  end

  def init(monster) do
    if monster.room_id do
      PubSub.subscribe(self, "rooms:#{monster.room_id}:monsters")
    end

    PubSub.subscribe(self, "monsters")

    {:ok, Map.put(monster, :pid, self)}
  end

  def execute_command(monster, command, arguments) do
    GenServer.cast(monster, {:execute_command, command, arguments})
  end

  def value(monster) do
    GenServer.call(monster, :value)
  end

  def insert(monster) do
    GenServer.call(monster, :insert)
  end

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
    case Repo.get(Monster, id) do
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

        {:ok, pid} = Supervisor.start_child(ApathyDrive.Supervisor, {:"monster_#{monster.id}", {GenServer, :start_link, [Monster, monster, [name: {:global, :"monster_#{id}"}]]}, :permanent, 5000, :worker, [Monster]})

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
    Map.put(monster, :room_id, room_id)
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

  def modified_stat(%Monster{} = monster, stat_name) do
    Map.get(monster, String.to_atom(stat_name), 0) +
    stat_skill_bonus(monster, stat_name) +
    effect_bonus(monster, stat_name)
  end

  def stat_skill_bonus(%Monster{} = monster, stat_name) do
    monster
    |> trained_skills
    |> Enum.map(&(Systems.Skill.find(to_string(&1))))
    |> Enum.filter(fn(skill) ->
         skill.modifiers
         |> Map.keys
         |> Enum.member?(stat_name)
       end)
    |> Enum.reduce(0, fn(skill, total_stat_modification) ->
         base = skill_from_training(monster, skill.name)
         percentage = skill.modifiers[stat_name] / (skill.modifiers
                                                    |> Map.values
                                                    |> Enum.sum)
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

  def base_skill(%Monster{skills: skills} = monster, skill_name) do
    base = skills
           |> Map.get(skill_name, %{})
           |> Map.get("base", 0)

    base + skill_from_training(monster, skill_name)
  end

  def modified_skill(%Monster{} = monster, skill_name) do
    skill = Systems.Skill.find(skill_name)

    base = base_skill(monster, skill_name)

    modified = if base > 0 do
      total = Map.keys(skill.modifiers) |> Enum.reduce(0, fn(stat, total) ->
                                       total + modified_stat(monster, stat) * Map.get(skill.modifiers, stat, 0)
                                     end)
      average = total / (Map.values(skill.modifiers) |> Enum.sum)

      round(base * (1 + average * 0.005))
    else
      0
    end
    (modified + effect_bonus(monster, skill_name))
  end

  def skill_from_training(%Monster{skills: skills}, skill_name) do
    skill = Systems.Skill.find(skill_name)

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

  def good?(%Monster{alignment: alignment}) do
    case Decimal.min(alignment, Decimal.new(-50)) do
      ^alignment ->
        true
      _ ->
        false
    end
  end

  def evil?(%Monster{alignment: alignment}) do
    case Decimal.max(alignment, Decimal.new(50)) do
      ^alignment ->
        true
      _ ->
        false
    end
  end

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

  def handle_call(:insert, _from, %Monster{id: nil} = monster) do
    monster = ApathyDrive.Repo.insert(monster)

    :global.register_name(:"monster_#{monster.id}", monster.pid)

    {:reply, monster, monster}
  end
  def handle_call(:insert, _from, monster) do
    {:reply, monster, monster}
  end

  def handle_cast({:execute_command, command, arguments}, monster) do
    monster = ApathyDrive.Command.execute(monster, command, arguments)
    {:noreply, monster}
  end

end
