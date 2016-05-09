defmodule MonsterTemplate do
  use ApathyDrive.Web, :model
  use GenServer
  use Timex

  alias ApathyDrive.{Mobile, Repo, Room}

  schema "monster_templates" do
    field :name,                   :string
    field :description,            :string
    field :death_message,          :string
    field :enter_message,          :string
    field :exit_message,           :string
    field :greeting,               :string
    field :gender,                 :string
    field :game_limit,             :integer
    field :adjectives,             {:array, :string}, default: []
    field :chance_to_follow,       :integer
    field :alignment,              :string
    field :level,                  :integer
    field :questions,              ApathyDrive.JSONB
    field :flags,                  {:array, :string}, default: []
    field :experience,             :integer
    field :last_killed_at,         Timex.Ecto.DateTime
    field :regen_time_in_minutes,  :integer
    field :permanent,              :boolean
    field :movement,               :string
    field :minimum_essence,        :integer, default: 0
    field :unities,                {:array, :string}

    has_many :monsters, Monster
    has_many :lairs, ApathyDrive.LairMonster
    has_many :lair_rooms, through: [:lairs, :room]
    has_many :monster_abilities, ApathyDrive.MonsterAbility
    has_many :abilities, through: [:monster_abilities, :ability]

    timestamps
  end

  def start_link(mt, opts \\ []) do
    GenServer.start_link(__MODULE__, mt, opts)
  end

  def questions(mt_id) do
    mt_id
    |> find()
    |> GenServer.call(:questions)
  end

  def init(mt) do
    {:ok, mt}
  end

  def changeset(%MonsterTemplate{} = monster_template, params \\ %{}) do
    monster_template
    |> cast(params, ~w(name description death_message enter_message enter_message exit_message greeting gender alignment level game_limit experience), ~w())
    |> validate_format(:name, ~r/^[a-zA-Z ,]+$/)
    |> validate_length(:name, min: 1, max: 30)
    |> validate_inclusion(:gender, MonsterTemplate.genders)
    |> validate_inclusion(:alignment, MonsterTemplate.alignments)
  end

  def find(id) do
    case :global.whereis_name("monster_template_#{id}") do
      :undefined ->
        load(id)
      monster_template ->
        monster_template
    end
  end

  def load(id) do
    import Supervisor.Spec
    monster_template =
      MonsterTemplate
      |> Repo.get(id)
      |> Repo.preload(:abilities)

    case Supervisor.start_child(ApathyDrive.Supervisor, worker(MonsterTemplate, [monster_template, [name: {:global, "monster_template_#{id}"}]], id: "monster_template_#{id}", restart: :permanent)) do
      {:error, {:already_started, pid}} ->
        pid
      {:ok, pid} ->
        pid
    end
  end

  def datalist do
    __MODULE__
    |> Repo.all
    |> Enum.map(fn(mt) ->
         "#{mt.name} - #{mt.id}"
       end)
  end

  def create_monster(monster_template_id, room) when is_integer(monster_template_id) do
    monster_template_id
    |> find
    |> create_monster(room)
  end

  def create_monster(monster_template, room) do
    GenServer.call(monster_template, {:create_monster, room})
  end

  def abilities(monster_template_id) do
    __MODULE__
    |> where(id: ^monster_template_id)
    |> Ecto.Query.preload(:abilities)
    |> Repo.one!
    |> Map.get(:abilities)
    |> Enum.map(&Map.get(&1, :properties))
  end

  def on_cooldown?(%MonsterTemplate{regen_time_in_minutes: nil}), do: false
  def on_cooldown?(%MonsterTemplate{last_killed_at: nil}),        do: false
  def on_cooldown?(%MonsterTemplate{regen_time_in_minutes: regen_time, last_killed_at: last_killed_at}) do
    respawn_at = DateTime.now
                 |> DateTime.shift(minutes: -regen_time)

    -1 == DateTime.compare(respawn_at, last_killed_at)
  end

  def limit_reached?(%MonsterTemplate{game_limit: game_limit} = monster_template) do
    count(monster_template) >= game_limit
  end

  def count(%MonsterTemplate{id: monster_template_id}) do
    ApathyDrive.Mobile
    |> where(monster_template_id: ^monster_template_id)
    |> select([m], count(m.id))
    |> Repo.one
  end

  def name_with_adjective(name, nil), do: name
  def name_with_adjective(name, []),  do: name
  def name_with_adjective(name, adjectives) do
    adjective = adjectives
                |> Enum.random

    "#{adjective} #{name}"
  end

  def value(monster) do
    GenServer.call(monster, :value)
  end

  def set_last_killed_at(%Mobile{monster_template_id: id}) do
    id
    |> find
    |> GenServer.cast(:set_last_killed_at)
  end

  def genders do
    [nil, "male", "female"]
  end

  def alignments do
    ["good", "neutral", "evil"]
  end

  def unity_alignment(["good"]), do: "good"
  def unity_alignment(["evil"]), do: "evil"
  def unity_alignment(_),       do: "neutral"

  def handle_call({:create_monster, %Room{} = room}, _from, monster_template) do
    monster = %{
      name: name_with_adjective(monster_template.name, monster_template.adjectives),
      description: monster_template.description,
      enter_message: monster_template.enter_message,
      exit_message: monster_template.exit_message,
      death_message: monster_template.death_message,
      room_id: room.id,
      gender: monster_template.gender,
      greeting: monster_template.greeting,
      monster_template_id: monster_template.id,
      flags: monster_template.flags,
      chance_to_follow: monster_template.chance_to_follow,
      movement: monster_template.movement,
      unities: monster_template.unities || (room |> Room.controlled_by |> List.wrap |> List.delete("default")),
      alignment: monster_template.alignment,
      spawned_at: room.id,
      minimum_essence: monster_template.minimum_essence || 0
    }

    monster = Map.put(monster, :keywords, String.split(monster.name))

    if length(monster.unities) > 0 do
      experience =
        Enum.reduce(monster.unities, 0, fn(unity, exp) ->
          exp + div(room.room_unity.essences[unity], length(monster.unities))
        end)

      monster =
        monster
        |> Map.put(:level, ApathyDrive.Level.level_at_exp(experience))
        |> Map.put(:experience, max(experience, monster_template.minimum_essence || 0))
        |> Map.put(:alignment, unity_alignment(monster.unities))
    else
      monster =
        monster
        |> Map.put(:level, monster_template.level)
        |> Map.put(:experience, max(ApathyDrive.Level.exp_at_level(monster_template.level), monster_template.minimum_essence || 0))
    end

    monster = Map.merge(%Mobile{}, monster)

    monster = Repo.save!(monster)

    {:reply, monster.id, monster_template}
  end

  def handle_call(:value, _from, monster_template) do
    {:reply, monster_template, monster_template}
  end

  def handle_call(:questions, _from, monster_template) do
    {:reply, monster_template.questions, monster_template}
  end

  def handle_cast(:set_last_killed_at, monster_template) do
    mt =
      monster_template
      |> Map.put(:last_killed_at, Timex.DateTime.now)
      |> Repo.save!

    {:noreply, mt}
  end

end
