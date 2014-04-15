defmodule ApathyDrive.Entity do
  use Ecto.Entity
  use Ecto.Model
  use Ecto.Query

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
    components = JSON.parse(entity_record.components)
    Enum.each components, fn(component) ->
      {component_name, component_values} = component
      ApathyDrive.Entity.add_component(entity, :"Elixir.Components.#{component_name}", component_values)
    end
    add_to_type_collection(entity)
    if Enum.member?(:gen_event.which_handlers(entity), Components.Help) do
      Systems.Help.add(entity)
    end
    :global.register_name(:"#{entity_record.id}", entity)
  end

  def list_components(entity) do
    :gen_event.which_handlers(entity)
  end

  def serialize_components(entity) do
    Enum.map(list_components(entity), fn (component) ->
      component.serialize(entity)
    end) |> Enum.reject(fn(component) ->
      component == nil
    end) |> JSON.generate
  end

  def save!(entity_pid) do
    if Enum.member?(list_components(entity_pid), Components.ID) do
      id = Components.ID.values(entity_pid)
      entity = Repo.get(ApathyDrive.Entity, id)
      entity.components(serialize_components(entity_pid))
      Repo.update(entity)
    else
      entity = ApathyDrive.Entity.new(components: serialize_components(entity_pid))
      Repo.create entity
      add_to_type_collection(entity_pid)
    end
  end

  def add_to_type_collection(entity) do
    if Enum.member?(:gen_event.which_handlers(entity), Components.Type) do
      case Components.Type.get_type(entity) do
        "race" ->
          Races.add(entity)
        "class" ->
          Classes.add(entity)
        "character" ->
          Characters.add(entity)
        "room" ->
          Systems.Room.initialize_lair_spawning(entity)
          Rooms.add(entity)
        "monster" ->
          Monsters.add(entity)
      end
    end
  end

end
