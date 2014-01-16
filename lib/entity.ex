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
    :gen_event.start_link()
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
    components = JSON.parse(entity_record.components)
    Enum.each components, fn(component) ->
      {component_name, component_values} = component
      ApathyDrive.Entity.add_component(entity, :"Elixir.Components.#{component_name}", component_values)
    end
    if Enum.member?(:gen_event.which_handlers(entity), Components.Type) do
      case Components.Type.get_type(entity) do
        "race" ->
          Races.add(entity)
      end
    end
    if Enum.member?(:gen_event.which_handlers(entity), Components.Help) do
      Systems.Help.add(entity)
    end
    :global.register_name(:"#{entity_record.id}", entity)
  end

end
