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
    field :skills,            ApathyDrive.JSONB
    field :chance_to_follow,  :integer
    field :disposition,       :string
    field :alignment,         :string
    field :level,  :integer
    field :questions,         ApathyDrive.JSONB
    field :flags,             {:array, :string}, default: []
    field :max_hp,            :integer
    field :max_mana,          :integer
    field :hp_regen,          :integer
    field :mana_regen,        :integer
    field :attacks,           ApathyDrive.JSONB
    field :effects,           ApathyDrive.JSONB
    field :experience,        :integer

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
    |> assoc(:monsters)
    |> Repo.all
    |> Enum.count
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

  def value(monster) do
    GenServer.call(monster, :value)
  end

  def handle_call({:spawn_monster, %Room{} = room}, _from, monster_template) do
    values = monster_template
             |> Map.from_struct
             |> Map.delete(:__meta__)
             |> Enum.into(Keyword.new)

    monster = struct(Monster, values)
              |> Map.put(:name, name_with_adjective(monster_template.name, monster_template.adjectives))
              |> Map.put(:monster_template_id, monster_template.id)
              |> Map.put(:id, nil)
              |> Map.put(:room_id, room.id)
              |> Map.put(:lair_id, room.id)
              |> Map.put(:hp, monster_template.max_hp)
              |> Map.put(:mana, monster_template.max_mana)
              |> Map.put(:effects, %{"monster_template" => monster_template.effects})

    monster = monster
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
