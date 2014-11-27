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

  def use!(item, monster) do
    case GenEvent.call(item, Components.Uses, :decrement) do
      nil ->
        nil
      uses when uses < 1 ->
        template = Components.Module.value(item)

        if template.destruct_message do
          send_message(monster, "scroll", "<p>#{template.destruct_message}</p>")
        end

        if template.room_destruct_message do
          monster
          |> Parent.of
          |> Systems.Room.characters_in_room
          |> Enum.each(fn(spirit) ->
               observer = Possession.possessed(spirit) || spirit

               if observer != monster do
                 send_message(monster, "scroll", "<p>#{template.room_destruct_message |> interpolate(%{"user" => monster})}</p>")
               end
             end)
        end

        Components.Items.remove_item(monster, item)
        Entities.save(monster)
        Entities.delete!(item)
      _uses ->
        Entities.save!(item)
        Entities.save!(monster)
        Entities.save!(Parent.of(monster))
    end
  end

  def serialize(entity) do
    %{"Uses" => value(entity)}
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
