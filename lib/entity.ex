defmodule ApathyDrive.Entity do
  use Ecto.Entity
  use Ecto.Model
  import Ecto.Query, only: [from: 2]

  field :components, :string

  queryable "entities" do
    field :components, :string
  end

  ### Public API
  def init do
    :gen_event.start()
  end

  def add_component(pid, component, args) do
    :gen_event.add_handler(pid, component, args)
    Components.add(component, pid)
  end

  def notify(pid, event) do
    :gen_event.notify(pid, event)
  end

  def load! do
    query = from e in ApathyDrive.Entity, limit: 500000, order_by: e.id, select: e
    Enum.each Repo.all(query), fn(entity) ->
      ApathyDrive.Entity.load!(entity)
    end
  end

  def load!(entity_record) do
    {:ok, entity} = ApathyDrive.Entity.init
    ApathyDrive.Entity.add_component(entity, Components.ID, entity_record.id)
    components = ExJSON.parse(entity_record.components, :to_map)
    Enum.each components, fn(component) ->
      {component_name, component_values} = component
      ApathyDrive.Entity.add_component(entity, :"Elixir.Components.#{component_name}", component_values)
    end
    if Enum.member?(:gen_event.which_handlers(entity), Components.Help) do
      Systems.Help.add(entity)
    end
  end

  def list_components(entity) do
    :gen_event.which_handlers(entity)
  end

  def serialize_components(entity) do
    Enum.map(list_components(entity), fn (component) ->
      component.serialize(entity)
    end) |> Enum.reject(fn(component) ->
      component == nil
    end) |> ExJSON.generate
  end

  def save!(entity_pid) do
    if Enum.member?(list_components(entity_pid), Components.ID) do
      id = Components.ID.value(entity_pid)
      entity = Repo.get(ApathyDrive.Entity, id)
      entity = entity.components(serialize_components(entity_pid))
      Repo.update(entity)
    else
      components = serialize_components(entity_pid)
      entity = ApathyDrive.Entity.new(components: components)
      entity = Repo.insert(entity)
      add_component(entity_pid, Components.ID, entity.id)
    end
  end

end
