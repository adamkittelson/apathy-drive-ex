defmodule Room do
  use Ecto.Model
  use Systems.Reload
  use GenServer
  alias ApathyDrive.Repo
  alias Phoenix.PubSub

  schema "rooms" do
    field :name,              :string
    field :keywords,          {:array, :string}
    field :description,       :string
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

  def exit_directions(room) do
    room
    |> exits
    |> Enum.map(fn(room_exit) ->
         :"Elixir.Systems.Exits.#{room_exit.kind}".display_direction(room, room_exit)
       end)
    |> Enum.reject(&(&1 == nil))
  end

  def shop?(room),    do: !!shop_items(room)
  def trainer?(room), do: !!trainable_skills(room)

  def items(room),    do: Phoenix.PubSub.subscribers("rooms:#{id(room)}:items")
  def monsters(room), do: Phoenix.PubSub.subscribers("rooms:#{id(room)}:monsters")

  def start_room_id do
    query = from r in Room,
            where: r.start_room == true,
            select: r.id

    Repo.one(query)
  end

  def find(id) do
    case :global.whereis_name(:"room_#{id}") do
      :undefined ->
        load(id)
      room ->
        room
    end
  end

  def load(id) do
    room = Repo.get(Room, id)
           |> parse_json(:item_descriptions)
           |> parse_json(:lair)
           |> parse_json(:exits)

    {:ok, pid} = Supervisor.start_child(:room_supervisor, {:"room_#{id}", {GenServer, :start_link, [Room, room, [name: {:global, :"room_#{id}"}]]}, :permanent, 5000, :worker, [Room]})
    PubSub.subscribe(pid, "rooms")
    pid
  end

  def all do
    PubSub.subscribers("rooms")
  end

  defp parse_json(room, attribute) do
    Map.put(room, attribute, Poison.decode!(Map.get(room, attribute), keys: :atoms))
  end

  @ecto_fields |> Enum.each(fn({name, _type, _opts}) ->
    def unquote(name)(room) do
      GenServer.call(room, unquote(name))
    end
  end)

  @ecto_fields |> Enum.each(fn({name, _type, _opts}) ->
    def handle_call(unquote(name), _from, room) do
      {:reply, Map.get(room, unquote(name)), room}
    end
  end)

end
