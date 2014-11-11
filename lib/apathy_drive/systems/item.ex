defmodule Systems.Item do
  use Systems.Reload
  import Utility

  def spawn_item(item) do
    {:ok, entity} = Entity.init
    Entity.list_components(item)
    |> Enum.reject(&(&1 == Components.ID))
    |> Enum.each fn(component) ->
      Entity.add_component(entity, component, component.value(item))
    end

    template = Components.Module.value(item)

    if template.worn_on do
      Entity.add_component(entity, Components.WornOn, template.worn_on)
    end

    if template.slot do
      Entity.add_component(entity, Components.Slot, template.slot)
    end

    Entity.add_component(entity, Components.AC, template.ac)

    Entity.add_component(entity, Components.Module, template)

    Entity.add_component(entity, Components.Effects, %{})

    Entity.add_component(entity, Components.Types, ["item"])
    entity
  end

  def spawn_item(item, entity) do
    item = spawn_item(item)
    Components.Items.add_item(entity, item)
    Entities.save(entity)
  end

  def has_item?(monster, item_name) do
    Components.Items.get_items(monster)
    |> Enum.map(&Components.Name.value/1)
    |> Enum.member?(item_name)
  end

  def skill_too_low(monster, item) do
    skills = Components.Module.value(item).required_skills

    skills
    |> Map.keys
    |> Enum.find(fn(skill) ->
         Systems.Skill.base(monster, skill) < skills[skill]
       end)
  end

  def display_inventory(character) do
    if Entity.has_component?(character, Components.Limbs) do
      limbs = Components.Limbs.value(character)
      equipped_items = Systems.Limbs.equipped_items(character)

      if equipped_items |> Enum.count > 0 do
        send_message(character, "scroll", "<p><span class='dark-yellow'>You are equipped with:</span></p><br>")
        equipped_items |> Enum.each fn(item) ->
          item_name = Components.Name.value(item)
          item_limbs = Systems.Limbs.get_limb_names(limbs, item)
          send_message(character, "scroll", "<p><span class='dark-green'>#{String.ljust(item_name, 20)}</span><span class='dark-cyan'>(#{Enum.join(item_limbs, ", ")})</span></p>")
        end
      end
    end

    items = Components.Items.get_items(character) |> Enum.map(&(Components.Name.value(&1)))
    if items |> Enum.count > 0 do
      send_message(character, "scroll", "<br><p>You are carrying #{Enum.join(items, ", ")}</p>")
    else
      send_message(character, "scroll", "<br><p>You are carrying nothing.</p>")
    end
  end

  def equip(character, item) do
    case Systems.Match.one(Components.Items.get_items(character), :name_contains, item) do
      nil ->
        send_message(character, "scroll", "<p>You don't have \"#{item}\" left unequipped.</p>")
      match ->
        equip(character, match, skill_too_low(character, match))
    end
  end

  def equip(monster, item, nil) do
    case Components.Limbs.equip(monster, item) do
      %{"removed" => items_removed} ->
        Enum.each(items_removed, fn(item_removed) ->
          Components.Items.add_item(monster, item_removed)
          send_message(monster, "scroll", "<p>You remove #{Components.Name.value(item_removed)}.</p>")
        end)
        Components.Items.remove_item(monster, item)
        send_message(monster, "scroll", "<p>You are now wearing #{Components.Name.value(item)}.</p>")
        Entities.save(monster)
      %{"error" => message} ->
        send_message(monster, "scroll", "<p>#{message}</p>")
      _ ->
        Components.Items.remove_item(monster, item)
        send_message(monster, "scroll", "<p>You are now wearing #{Components.Name.value(item)}.</p>")
        Entities.save(monster)
    end
  end

  def equip(monster, item, skill) do
    send_message(monster, "scroll", "<p>You don't have enough #{skill} skill to equip that.</p>")
  end

  def unequip(character, item) do
    case Systems.Match.one(Systems.Limbs.equipped_items(character), :name_contains, item) do
      nil ->
        send_message(character, "scroll", "<p>You are not wearing \"#{item}\".</p>")
      match ->
        Components.Limbs.unequip(character, match)
        Components.Items.add_item(character, match)
        send_message(character, "scroll", "<p>You remove #{Components.Name.value(match)}.</p>")
        Entities.save(character)
    end
  end

  defmacro __using__(_opts) do
    quote do
      use Systems.Reload
      import Systems.Text
      import Utility
      import BlockTimer

      def name do
        __MODULE__
        |> Atom.to_string
        |> String.split(".")
        |> List.last
        |> Inflex.underscore
        |> String.replace("_", " ")
      end

      def keywords do
        (name |> String.split)
      end

      def description,     do: nil
      def slot,            do: nil
      def worn_on,         do: nil
      def hit_verbs,       do: nil
      def damage,          do: nil
      def required_skills, do: %{}
      def speed,           do: nil
      def ac,              do: 0

      def weapon?, do: slot == "weapon" and !!hit_verbs


      defoverridable [name: 0,
                      keywords: 0,
                      description: 0,
                      slot: 0,
                      worn_on: 0,
                      hit_verbs: 0,
                      damage: 0,
                      required_skills: 0,
                      speed: 0,
                      ac: 0]
    end
  end


end
