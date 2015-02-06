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
    field :skills,              :string, default: %{base: %{}, trained: %{}} #json
    field :limbs,               :string #json
    field :monster_template_id, :integer
    field :room_id,             :integer
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
    field :abilities,           :any,     virtual: true, default: []
    field :greeting,            :string,  virtual: true
    field :gender,              :string,  virtual: true
    field :strength,            :integer, virtual: true, default: 0
    field :agility,             :integer, virtual: true, default: 0
    field :intelligence,        :integer, virtual: true, default: 0
    field :health,              :integer, virtual: true, default: 0
    field :hit_verbs,           :any,     virtual: true
    field :chance_to_follow,    :integer, virtual: true
    field :damage,              :any,     virtual: true
    field :possession_level,    :integer, virtual: true
    field :questions,           :any,     virtual: true
    field :pid,                 :any,     virtual: true
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

  def find_room(%Monster{room_id: room_id} ) do
    room_id
    |> Room.find
    |> Room.value
  end

  def find_room(%Monster{room_id: room_id}) do
    room_id
    |> Room.find
    |> Room.value
  end

  def max_hp(%Monster{} = monster) do
    health   = modified_stat(monster, :health)
    strength = modified_stat(monster, :strength)

    seed = trunc((health * 2 + strength) / 3)

    trunc(seed * (11 + (seed / 10)))
  end

  def modified_stat(%Monster{} = monster, stat_name) do
    Map.get(monster, stat_name, 0) +
    stat_skill_bonus(monster, stat_name) +
    effect_bonus(monster, stat_name)
  end

  def stat_skill_bonus(%Monster{skills: %{trained: skills}} = monster, stat_name) do
    skills
    |> Map.keys
    |> Enum.map(&(Systems.Skill.find(to_string(&1))))
    |> Enum.filter(fn(skill) ->
         skill.modifiers
         |> Map.keys
         |> Enum.member?(stat_name)
       end)
    |> Enum.reduce(0, fn(skill, total_stat_modification) ->
         base = skill_from_training(monster, skill.name |> String.to_atom)
         percentage = skill.modifiers[stat_name] / (skill.modifiers
                                                    |> Map.values
                                                    |> Enum.sum)
         total_stat_modification + base * percentage
       end)
    |> trunc
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

  def base_skill(%Monster{skills: %{base: base}} = monster, skill_name) do
    Map.get(base, skill_name, 0) + skill_from_training(monster, skill_name)
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

  def skill_from_training(%Monster{skills: %{base: base, trained: skills}}, skill_name) do
    skill = Systems.Skill.find(skill_name)

    power_spent = Map.get(skills, skill_name, 0)
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

  def display_enter_message(%Room{} = room, monster) do
    display_enter_message(room, monster, Room.random_direction(room))
  end

  def display_enter_message(%Room{} = room, monster, direction) do
    message = monster
              |> enter_message
              |> interpolate(%{
                   "name" => name(monster),
                   "direction" => Room.enter_direction(direction)
                 })
              |> capitalize_first

    Phoenix.Channel.broadcast "rooms:#{room.id}", "scroll", %{:html => "<p><span class='dark-green'>#{message}</span></p>"}
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

  def handle_call(:value, _from, monster) do
    {:reply, monster, monster}
  end

  def handle_call(:keywords, _from, monster) do
    {:reply, String.split(monster.name), monster}
  end

  def handle_call(:insert, _from, %Monster{id: nil} = monster) do
    monster = monster
              |> Map.put(:skills, Poison.encode!(monster.skills))
              |> Map.put(:limbs,  Poison.encode!(monster.limbs))
              |> ApathyDrive.Repo.insert
              |> Map.put(:skills, monster.skills)
              |> Map.put(:limbs,  monster.limbs)

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
