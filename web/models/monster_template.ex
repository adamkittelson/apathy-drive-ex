defmodule MonsterTemplate do
  use Ecto.Model
  use GenServer

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
    field :skills,            ApathyDrive.JSONB
    field :hit_verbs,         {:array, :string}, default: ["attack", "assault", "strike"]
    field :limbs,             ApathyDrive.JSONB
    field :chance_to_follow,  :integer
    field :damage,            ApathyDrive.JSONB
    field :disposition,       :string
    field :alignment,         :string
    field :possession_level,  :integer
    field :questions,         ApathyDrive.JSONB

    has_many :monsters, Monster

    timestamps
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

    {:ok, pid} = Supervisor.start_child(ApathyDrive.Supervisor, {:"monster_template_#{id}", {GenServer, :start_link, [MonsterTemplate, monster_template, [name: {:global, :"monster_template_#{id}"}]]}, :permanent, 5000, :worker, [MonsterTemplate]})

    pid
  end

  def spawn_monster(monster_template_id, room) when is_integer(monster_template_id) do
    monster_template_id
    |> find
    |> spawn_monster(room)
  end

  def spawn_monster(monster_template, room) do
    GenServer.call(monster_template, {:spawn_monster, room})
  end

  def limit_reached?(%MonsterTemplate{game_limit: game_limit} = monster_template) do
    count(monster_template) >= game_limit
  end

  def count(%MonsterTemplate{} = monster_template) do
    monster_template
    |> monsters
    |> HashSet.size
  end

  def monsters(%MonsterTemplate{id: id}) do
    Phoenix.PubSub.subscribers(ApathyDrive.PubSub , "monster_template:#{id}")
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
        -75
      "neutral" ->
        0
      "evil" ->
        75
    end
    alignment
  end

  def value(monster) do
    GenServer.call(monster, :value)
  end

  def handle_call({:spawn_monster, %Room{} = room}, _from, monster_template) do
    values = monster_template
             |> Map.from_struct
             |> Enum.into(Keyword.new)

    skills = Map.get(monster_template, :skills, %{})

    skills = skills
             |> Map.keys
             |> Enum.reduce(%{}, fn(skill_name, monster_skills) ->
                  Map.put(monster_skills, skill_name, %{"base" => skills[skill_name]})
                end)

    monster = struct(Monster, values)
              |> Map.put(:name, name_with_adjective(monster_template.name, monster_template.adjectives))
              |> Map.put(:monster_template_id, monster_template.id)
              |> Map.put(:id, nil)
              |> Map.put(:alignment, alignment(monster_template.alignment))
              |> Map.put(:room_id, room.id)
              |> Map.put(:skills, skills)

    monster = monster
              |> Map.put(:hp, Monster.max_hp(monster))
              |> Map.put(:mana, Monster.max_mana(monster))
              |> Map.put(:keywords, String.split(monster.name))
              |> Monster.insert

    worker_id = :"monster_#{monster.id}"

    {:ok, pid} = Supervisor.start_child(ApathyDrive.Supervisor, {worker_id, {GenServer, :start_link, [Monster, monster, []]}, :transient, 5000, :worker, [Monster]})

    {:reply, pid, monster_template}
  end

  def handle_call(:value, _from, monster_template) do
    {:reply, monster_template, monster_template}
  end

end
