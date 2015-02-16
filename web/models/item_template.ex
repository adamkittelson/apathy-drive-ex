defmodule ItemTemplate do
  use Ecto.Model
  use Systems.Reload
  use GenServer
  alias ApathyDrive.Repo
  alias Phoenix.PubSub

  schema "item_templates" do
    field :name,                  :string
    field :keywords,              {:array, :string}, default: []
    field :description,           :string
    field :slot,                  :string
    field :worn_on,               ApathyDrive.JSONB
    field :hit_verbs,             {:array, :string}, default: []
    field :damage,                ApathyDrive.JSONB
    field :required_skills,       ApathyDrive.JSONB
    field :speed,                 :float
    field :accuracy_skill,        :string
    field :ac,                    :integer
    field :uses,                  :integer
    field :destruct_message,      :string
    field :room_destruct_message, :string
    field :can_pick_up,           :boolean
    field :cost,                  :integer
    field :light,                 :integer
    field :light_duration,        :integer
    field :always_lit,            :boolean

    timestamps
  end

  def value(item_template) do
    GenServer.call(item_template, :value)
  end

  def find(id) do
    case :global.whereis_name(:"item_template_#{id}") do
      :undefined ->
        load(id)
      item_template ->
        item_template
    end
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

  def handle_call(:value, _from, item_template) do
    {:reply, item_template, item_template}
  end
end
