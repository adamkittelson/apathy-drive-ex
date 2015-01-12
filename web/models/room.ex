defmodule Room do
  use Ecto.Model
  use GenServer
  use Systems.Reload
  alias ApathyDrive.Repo

  schema "rooms" do
    field :name,              :string
    field :keywords,          {:array, :string}
    field :description,       :string
    field :monsters,          :any, default: [], virtual: true
    field :items,             :any, default: [], virtual: true
    field :spirits,           :any, default: [], virtual: true
    field :light,             :integer
    field :item_descriptions, :string #json
    field :placed_items,      {:array, :string}
    field :lair,              :string #json
    field :permanent_npc,     :string
    field :room_ability,      :string
    field :start_room,        :boolean, default: false
    field :shop_items,        {:array, :string}
    field :trainable_skills,  {:array, :string}
    field :exits,             :string #json
    field :legacy_id,         :string
    field :created_at,        :datetime
    field :updated_at,        :datetime
  end

  def value(room) do
    GenServer.call(room, :value)
  end

  def shop?(room),    do: !!Room.value(room).shop_items
  def trainer?(room), do: !!Room.value(room).trainable_skills

  def add_spirit(room, spirit) do
    GenServer.call(room, {:add_spirit, spirit})
  end

  def start_room_id do
    query = from r in Room,
            where: r.start_room == true,
            select: r.id

    Repo.one(query)
  end

  def load(id) do
    room = Repo.get(Room, id)
           |> parse_json(:item_descriptions)
           |> parse_json(:lair)
           |> parse_json(:exits)

    Supervisor.start_child(:room_supervisor, {:"room_#{id}", {GenServer, :start_link, [Room, room]}, :permanent, 5000, :worker, [Room]})
  end

  defp parse_json(room, attribute) do
    Map.put(room, attribute, Poison.decode!(Map.get(room, attribute), keys: :atoms))
  end

  def handle_call(:value, _from, room) do
    {:reply, room, room}
  end

  def handle_call({:add_spirit, spirit}, _from, room) do
    Parent.set(spirit, self)
    room = Map.put(room, :spirits, [spirit | room.spirits] |> Enum.uniq)
    {:reply, room, room}
  end

  def handle_call(:name, _from, room) do
    {:reply, room.name, room}
  end

end