defmodule Entities do
  use Systems.Reload
  use Ecto.Model
  import Ecto.Query, only: [from: 2]
  alias Poison, as: JSON

  schema "entities" do
    field :components, :string
  end

  def load! do
    query = from e in Entities, limit: 500000, order_by: e.id, select: e
    Enum.each Repo.all(query), fn(entity) ->
      load!(entity)
    end
  end

  def load!(entity_record) do
    {:ok, entity} = Entity.init
    Entity.add_component(entity, Components.ID, entity_record.id)
    components = JSON.decode!(entity_record.components)
    Enum.each components, fn(component) ->
      {component_name, component_values} = component
      Entity.add_component(entity, :"Elixir.Components.#{component_name}", component_values)
    end
    Entity.add_to_type_collection(entity)
    if Entity.has_component?(entity, Components.Help) do
      Help.add(entity)
    end
  end

  def save!(entity_pid) do
    if Mix.env != :test do
      if Enum.member?(Entity.list_components(entity_pid), Components.ID) do
        id = Components.ID.value(entity_pid)
        entity = Repo.get(Entities, id)
        entity = %{ entity | components: Entity.serialize_components(entity_pid)}
        Repo.update(entity)
      else
        components = Entity.serialize_components(entity_pid)
        entity = %Entities{components: components}
        entity = Repo.insert(entity)
        Entity.add_component(entity_pid, Components.ID, entity.id)
        Entity.add_to_type_collection(entity_pid)
      end
    end
  end

  def delete!(entity_pid) do
    if Enum.member?(Entity.list_components(entity_pid), Components.ID) do
      id = Components.ID.value(entity_pid)
      entity = Repo.get(Entities, id)
      Repo.delete(entity)
    end
    Entity.remove_from_type_collection(entity_pid)
    Entity.list_components(entity_pid) |> Enum.each(&(Entity.remove_component(entity_pid, &1)))
    Parent.set(entity_pid, nil)
    GenEvent.stop(entity_pid)
  end

end
