defmodule ItemTemplate do
  use Ecto.Model

  use GenServer
  alias ApathyDrive.Repo
  alias ApathyDrive.PubSub

  schema "item_templates" do
    field :name,                  :string
    field :keywords,              {:array, :string}, default: []
    field :description,           :string
    field :worn_on,               :string
    field :hit_verbs,             {:array, :string}, default: []
    field :properties,            ApathyDrive.JSONB
    field :required_skills,       ApathyDrive.JSONB
    field :speed,                 :integer
    field :accuracy_skill,        :string
    field :ac,                    :integer
    field :uses,                  :integer
    field :destruct_message,      :string
    field :room_destruct_message, :string
    field :can_pick_up,           :boolean
    field :cost,                  :integer
    field :light,                 :integer
    field :always_lit,            :boolean
    field :weight,                :integer

    timestamps
  end

  def value(item_template) do
    GenServer.call(item_template, :value)
  end

  def find(nil), do: nil
  def find(id) do
    case :global.whereis_name(:"item_template_#{id}") do
      :undefined ->
        load(id)
      item_template ->
        item_template
    end
  end

  def find_by_name(name) do
    search_string = "%#{name}%"
    query = from it in ItemTemplate, where: like(it.name, ^search_string),
                                     limit: 1,
                                     select: it.id
    Repo.one(query)
  end

  def load(id) do
    case Repo.get(ItemTemplate, id) do
      %ItemTemplate{} = item_template ->

        {:ok, pid} = Supervisor.start_child(ApathyDrive.Supervisor, {:"item_template_#{id}", {GenServer, :start_link, [ItemTemplate, item_template, [name: {:global, :"item_template_#{id}"}]]}, :permanent, 5000, :worker, [ItemTemplate]})

        PubSub.subscribe(pid, "item_templates")

        pid
      nil ->
        nil
    end
  end

  def spawn_item(item_template, %Monster{} = monster) do
    GenServer.call(item_template, :spawn_item)
    |> Item.to_monster_inventory(monster)
  end

  def spawn_item(item_template, %Room{} = room) do
    GenServer.call(item_template, :spawn_item)
    |> Item.to_room(room)
  end

  def spawn_item(item_template_id) when is_integer(item_template_id) do
    item_template_id
    |> find
    |> spawn_item
  end

  def spawn_item(item_template) do
    GenServer.call(item_template, :spawn_item)
  end

  def skill_too_low?(%Monster{}, %{required_skills: nil}), do: false
  def skill_too_low?(%Monster{} = monster, %{required_skills: %{} = required_skills}) do
    skill = required_skills
            |> Map.keys
            |> Enum.find(fn(skill) ->
                 Monster.modified_skill(monster, skill) < required_skills[skill]
               end)

    if skill do
      {skill, required_skills[skill]}
    end
  end

  # Generate functions from Ecto schema
  fields = Keyword.keys(@struct_fields) -- Keyword.keys(@ecto_assocs)

  Enum.each(fields, fn(field) ->
    def unquote(field)(item_template) do
      GenServer.call(item_template, unquote(field))
    end
  end)

  Enum.each(fields, fn(field) ->
    def handle_call(unquote(field), _from, item_template) do
      {:reply, Map.get(item_template, unquote(field)), item_template}
    end
  end)

  def handle_call(:spawn_item, _from, item_template) do
    values = item_template
             |> Map.from_struct
             |> Enum.into(Keyword.new)

    item = struct(Item, values)
           |> Map.put(:item_template_id, item_template.id)
           |> Map.put(:id, nil)
           |> Item.insert

    # if item_template.always_lit do
    #   Item.light(pid)
    # end

    {:reply, item, item_template}
  end

  def handle_call(:value, _from, item_template) do
    {:reply, item_template, item_template}
  end
end
