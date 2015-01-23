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
                       |> parse_json(:skills)
                       |> parse_json(:limbs)
                       |> parse_json(:damage)
                       |> parse_json(:questions)

    {:ok, pid} = Supervisor.start_child(:monster_template_supervisor, {:"monster_template_#{id}", {GenServer, :start_link, [MonsterTemplate, monster_template, [name: {:global, :"monster_template_#{id}"}]]}, :permanent, 5000, :worker, [MonsterTemplate]})

    pid
  end

  def parse_json(room, attribute) do
    Map.put(room, attribute, Poison.decode!(Map.get(room, attribute), keys: :atoms))
  end

  def spawn_monster(monster_template_id) when is_integer(monster_template_id) do
    monster_template_id
    |> find
    |> spawn_monster
  end

  def spawn_monster(monster_template) do
    GenServer.call(monster_template, {:spawn_monster, count(monster_template)})
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
    case alignment do
      "good" ->
        -75.0
      "neutral" ->
        0.0
      "evil" ->
        75.0
    end
  end

  # Generate functions from Ecto schema

  fields = Keyword.keys(@assign_fields)

  Enum.each(fields, fn(field) ->
    def unquote(field)(monster_template) do
      GenServer.call(monster_template, unquote(field))
    end
  end)

  Enum.each(fields, fn(field) ->
    def handle_call(unquote(field), _from, monster_template) do
      {:reply, Map.get(monster_template, unquote(field)), monster_template}
    end
  end)

  def handle_call({:spawn_monster, count}, _from, %MonsterTemplate{game_limit: game_limit} = monster_template) when count >= game_limit do
    {:reply, nil, monster_template}
  end

  def handle_call({:spawn_monster, count}, _from, monster_template) do
    values = monster_template
             |> Map.from_struct
             |> Enum.into(Keyword.new)

    monster = struct(Monster, values)
              |> Map.put(:name, name_with_adjective(monster_template.name, monster_template.adjectives))
              |> Map.put(:monster_template_id, monster_template.id)
              |> Map.put(:id, nil)
              |> Map.put(:alignment, alignment(monster_template.alignment))



    {:reply, monster, monster_template}
  end

end
