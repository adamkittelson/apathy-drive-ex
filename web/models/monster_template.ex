defmodule ApathyDrive.MonsterTemplate do
  use ApathyDrive.Web, :model
  use GenServer
  use Timex

  alias ApathyDrive.{Mobile, Monster, MonsterTemplate, Repo, Room}

  schema "monster_templates" do
    field :name,                   :string
    field :description,            :string
    field :death_message,          :string
    field :enter_message,          :string
    field :exit_message,           :string
    field :greeting,               :string
    field :gender,                 :string
    field :game_limit,             :integer
    field :adjectives,             ApathyDrive.JSONB, default: []
    field :chance_to_follow,       :integer
    field :alignment,              :string
    field :level,                  :integer
    field :questions,              ApathyDrive.JSONB
    field :flags,                  ApathyDrive.JSONB, default: []
    field :experience,             :integer
    field :movement,               :string
    field :unities,                ApathyDrive.JSONB
    field :limbs,                  ApathyDrive.JSONB
    field :abilities,              ApathyDrive.JSONB

    has_many :mobiles, Mobile
    has_many :lairs, ApathyDrive.LairMonster
    has_many :lair_rooms, through: [:lairs, :room]

    timestamps
  end

  def changeset(%MonsterTemplate{} = monster_template, params \\ %{}) do
    monster_template
    |> cast(params, ~w(name description death_message enter_message enter_message exit_message greeting adjectives alignment level experience movement), ~w(gender game_limit chance_to_follow unities))
    |> validate_format(:name, ~r/^[a-zA-Z ,]+$/)
    |> validate_length(:name, min: 1, max: 30)
    |> validate_inclusion(:gender, MonsterTemplate.genders)
    |> validate_inclusion(:alignment, MonsterTemplate.alignments)
  end

  def start_link(mt, opts \\ []) do
    GenServer.start_link(__MODULE__, mt, opts)
  end

  def questions(mt_id) do
    mt_id
    |> find()
    |> GenServer.call(:questions)
  end

  def abilities(mt_id) do
    MonsterTemplate
    |> Ecto.Query.where([mt], mt.id == ^mt_id)
    |> Ecto.Query.select([mt], mt.abilities)
    |> Repo.one
  end

  def init(mt) do
    {:ok, mt}
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

  def genders do
    [nil, "male", "female"]
  end

  def alignments do
    ["good", "neutral", "evil"]
  end

  def movements do
    ["stationary", "solo", "leader", "follower"]
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
      area_spawned_in: room.area_id,
      limbs: monster_template.limbs || %{
        "head" => %{"fatal" => true, "kind" => "head"},
        "left arm" => %{"kind" => "arm"},
        "right arm" => %{"kind" => "arm"},
        "left leg" => %{"kind" => "leg"},
        "right leg" => %{"kind" => "leg"},
      }
    }

    monster = Map.put(monster, :keywords, String.split(monster.name))

    monster =
      if length(monster.unities) > 0 do
        experience =
          Enum.reduce(monster.unities, 0, fn(unity, exp) ->
            exp + div(trunc(room.room_unity.essences[unity]), length(monster.unities))
          end)

        monster
        |> Map.put(:level, ApathyDrive.Level.level_at_exp(experience))
        |> Map.put(:experience, experience)
        |> Map.put(:alignment, unity_alignment(monster.unities))
      else
        experience = trunc(room.room_unity.essences["default"])

        monster
        |> Map.put(:level, ApathyDrive.Level.level_at_exp(experience))
        |> Map.put(:experience, experience)
      end

    monster = Map.merge(%Mobile{}, monster)

    monster = Repo.save!(monster)

    {:reply, monster, monster_template}
  end

  def handle_call(:value, _from, monster_template) do
    {:reply, monster_template, monster_template}
  end

  def handle_call(:questions, _from, monster_template) do
    {:reply, monster_template.questions, monster_template}
  end

end
