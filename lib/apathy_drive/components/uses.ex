defmodule Components.Uses do
  use Systems.Reload
  use GenEvent
  import Utility
  import Systems.Text

  ### Public API
  def value(entity) do
    GenEvent.call(entity, Components.Uses, :value)
  end

  def value(entity, new_value) do
    GenEvent.notify(entity, {:set_uses, new_value})
  end

  def use!(item) do
    parent = Parent.of(item)

    case GenEvent.call(item, Components.Uses, :decrement) do
      nil ->
        nil
      uses when uses < 1 ->
        template = Components.Module.value(item)

        if template.destruct_message && monster?(parent) do
          send_message(parent, "scroll", "<p>#{template.destruct_message}</p>")
        end

        if template.room_destruct_message do
          room = if room?(parent), do: parent, else: Parent.of(parent)

          room
          |> Systems.Room.characters_in_room
          |> Enum.each(fn(spirit) ->
               observer = Possession.possessed(spirit) || spirit

               if observer != parent do
                 send_message(parent, "scroll", "<p>#{template.room_destruct_message |> interpolate(%{"user" => parent})}</p>")
               end
             end)
        end

        Components.Items.remove_item(parent, item)
        Entities.save(parent)
        Entities.delete!(item)
      _uses ->
        Entities.save!(item)
        Entities.save(parent)
        if monster?(parent) do
          Entities.save!(Parent.of(parent))
        end
    end
  end

  def serialize(entity) do
    %{"Uses" => value(entity)}
  end

  defp room?(entity) do
    Entity.has_component?(entity, Components.Exits)
  end

  defp monster?(entity) do
    !room?(entity)
  end

  ### GenEvent API
  def init(value) do
    {:ok, value}
  end

  def handle_call(:value, value) do
    {:ok, value, value}
  end

  def handle_call(:decrement, value) when is_integer(value) do
    value = value - 1
    {:ok, value, value}
  end

  def handle_call(:decrement, value) do
    {:ok, value, value}
  end

  def handle_event({:set_level, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event(_, current_value) do
    {:ok, current_value}
  end
end
