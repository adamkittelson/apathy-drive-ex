defmodule MonsterTemplate do
  use Ecto.Model
  use GenServer
  use Systems.Reload
  alias ApathyDrive.Repo

  schema "monster_templates" do
    field :name,              :string
    field :description,       :string
    field :death_message,     :string
    field :enter_message,     :string
    field :exit_message,      :string
    field :abilities,         {:array, :integer}, default: []
    field :greeting,          :string
    field :gender,            :string
    field :game_limit,        :integer
    field :adjectives,        {:array, :string}, default: []
    field :strength,          :integer
    field :agility,           :integer
    field :intelligence,      :integer
    field :health,            :integer
    field :skills,            :string #json
    field :hit_verbs,         {:array, :string}, default: ["attack", "assault", "strike"]
    field :limbs,             :string #json
    field :chance_to_follow,  :integer
    field :damage,            :string #json
    field :disposition,       :string
    field :alignment,         :string
    field :possession_level,  :integer
    field :questions,         :string #json
  end

  def find(id) do
    case :global.whereis_name(:"monster_template_#{id}") do
      :undefined ->
        load(id)
      monster_template ->
        monster_template
    end
  end

  def load(id) do
    monster_template = Repo.get(MonsterTemplate, id)
                       |> parse_json(:damage)
                       |> parse_json(:questions)
                       |> parse_json(:skills)
                       |> parse_json(:limbs)

    {:ok, pid} = Supervisor.start_child(:monster_template_supervisor, {:"monster_template_#{id}", {GenServer, :start_link, [MonsterTemplate, monster_template, [name: {:global, :"monster_template_#{id}"}]]}, :permanent, 5000, :worker, [MonsterTemplate]})

    pid
  end

  def parse_json(room, attribute) do
    Map.put(room, attribute, Poison.decode!(Map.get(room, attribute), keys: :atoms))
  end

  def spawn_monster(monster_template_id, room) when is_integer(monster_template_id) do
    monster_template_id
    |> find
    |> spawn_monster(room)
  end

  def spawn_monster(monster_template, room) do
    GenServer.call(monster_template, {:spawn_monster, room})
  end

  def limit_reached?(monster_template) do
    count(monster_template) >= game_limit(monster_template)
  end

  def count(monster_template) do
    monster_template
    |> monsters
    |> length
  end

  def monsters(monster_template) do
    Phoenix.PubSub.subscribers("monster_template:#{id(monster_template)}")
  end

  def name_with_adjective(name, nil), do: name
  def name_with_adjective(name, []),  do: name
  def name_with_adjective(name, adjectives) do
    :random.seed(:os.timestamp)

    adjective = adjectives
                |> Enum.shuffle
                |> List.first

    "#{adjective} #{name}"
  end

  def alignment(alignment) do
    alignment = case alignment do
      "good" ->
        -75.0
      "neutral" ->
        0.0
      "evil" ->
        75.0
    end
    Decimal.new(alignment)
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

  def handle_call({:spawn_monster, room}, _from, monster_template) do
    values = monster_template
             |> Map.from_struct
             |> Enum.into(Keyword.new)

    monster = struct(Monster, values)
              |> Map.put(:name, name_with_adjective(monster_template.name, monster_template.adjectives))
              |> Map.put(:monster_template_id, monster_template.id)
              |> Map.put(:id, nil)
              |> Map.put(:alignment, alignment(monster_template.alignment))
              |> Map.put(:room_id, room.id)

    worker_id = :"monster_#{Systems.URL.random}"

    {:ok, pid} = Supervisor.start_child(:monster_supervisor, {worker_id, {GenServer, :start_link, [Monster, monster, []]}, :permanent, 5000, :worker, [Monster]})

    {:reply, pid, monster_template}
  end

end
