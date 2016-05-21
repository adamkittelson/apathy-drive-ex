defmodule ApathyDrive.Room do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Match, Mobile, Room, RoomUnity}

  schema "rooms" do
    field :name,                  :string
    field :keywords,              {:array, :string}
    field :description,           :string
    field :effects,               :map, virtual: true, default: %{}
    field :light,                 :integer
    field :item_descriptions,     ApathyDrive.JSONB, default: %{"hidden" => %{}, "visible" => %{}}
    field :lair_size,             :integer
    field :lair_frequency,        :integer, default: 5
    field :lair_next_spawn_at,    :integer, virtual: true, default: 0
    field :exits,                 ApathyDrive.JSONB, default: []
    field :commands,              ApathyDrive.JSONB, default: %{}
    field :legacy_id,             :string
    field :timers,                :map, virtual: true, default: %{}
    field :room_ability,          :any, virtual: true
    field :items,                 ApathyDrive.JSONB, default: []
    field :last_effect_key,       :integer, virtual: true, default: 0
    field :also_here,             :map, virtual: true, default: %{}
    field :area,                  :string
    field :default_essence,       :integer, virtual: true
    field :adjacent,              {:array, :map}, virtual: true, default: []

    timestamps

    has_one    :room_unity, RoomUnity
    has_many   :mobiles, Mobile
    belongs_to :ability, Ability
    has_many   :lairs, ApathyDrive.LairMonster
    has_many   :lair_monsters, through: [:lairs, :monster_template]
  end

  def essence_reaction(%Room{room_unity: %RoomUnity{essences: %{"good" => good, "evil" => evil}}} = room) when good > 0 and evil > 0 do
    good_to_remove =
      good
      |> div(50)
      |> max(1)

    evil_to_remove =
      evil
      |> div(50)
      |> max(1)

    update_in(room.room_unity.essences, &(&1 |> Map.put("good", good - good_to_remove) |> Map.put("evil", evil - evil_to_remove)))
  end
  def essence_reaction(%Room{} = room), do: room

  def controlled_by(%Room{} = room) do
    room.room_unity.controlled_by
  end

  def adjacent_room_data(%Room{} = room, data \\ %{}) do
    Map.merge(data, %{
      essences: room.room_unity.essences,
      room_id: room.id,
      area: room.area,
      controlled_by: room.room_unity.controlled_by
    })
  end

  def update_controlled_by(%Room{room_unity: %RoomUnity{essences: essences}} = room) do
    controlled_by = room.room_unity.controlled_by

    highest_essence =
      essences
      |> Map.keys
      |> Enum.sort_by(&Map.get(essences, &1, 0), &>=/2)
      |> List.first

    new_controlled_by =
      cond do
        highest_essence == "good" and (essences["good"] * 0.9) > essences["evil"] ->
          "good"
        highest_essence == "evil" and (essences["evil"] * 0.9) > essences["good"] ->
          "evil"
        true ->
          nil
      end

    if controlled_by != new_controlled_by do
      put_in(room.room_unity.controlled_by, new_controlled_by)
      |> Repo.save
    else
      room
    end
  end

  def default_essence(%Room{} = room) do
    room
    |> assoc(:lair_monsters)
    |> Repo.all
    |> default_essence()
  end
  def default_essence([]), do: 0
  def default_essence(lair_monsters) do
    total =
      lair_monsters
      |> Enum.map(fn(mt) ->
           ApathyDrive.Level.exp_at_level(mt.level)
         end)
      |> Enum.sum
    div(total, length(lair_monsters))
  end

  def changeset(%Room{} = room, params \\ %{}) do
    room
    |> cast(params, ~w(name description exits), ~w(light item_descriptions lair_size lair_frequency commands legacy_id))
    |> validate_format(:name, ~r/^[a-zA-Z ,]+$/)
    |> validate_length(:name, min: 1, max: 30)
  end

  def find_mobile_in_room(%Room{also_here: mobiles}, mobile, query) do
    mobile =
      mobiles
      |> Map.values
      |> Enum.find(&(&1.pid == mobile))

    mobiles
    |> Map.values
    |> Enum.reject(&(&1 == mobile))
    |> List.insert_at(-1, mobile)
    |> Match.one(:name_contains, query)
  end

  def datalist do
    __MODULE__
    |> Repo.all
    |> Enum.map(fn(mt) ->
         "#{mt.name} - #{mt.id}"
       end)
  end

  def start_room_id do
    ApathyDrive.Config.get(:start_room)
  end

  def find_item(%Room{items: items, item_descriptions: item_descriptions}, item) do
    actual_item = items
                  |> Enum.map(&(%{name: &1["name"], keywords: String.split(&1["name"]), item: &1}))
                  |> Match.one(:keyword_starts_with, item)

    visible_item = item_descriptions["visible"]
                   |> Map.keys
                   |> Enum.map(&(%{name: &1, keywords: String.split(&1)}))
                   |> Match.one(:keyword_starts_with, item)

    hidden_item = item_descriptions["hidden"]
                  |> Map.keys
                  |> Enum.map(&(%{name: &1, keywords: String.split(&1)}))
                  |> Match.one(:keyword_starts_with, item)

    cond do
      visible_item ->
        %{description: item_descriptions["visible"][visible_item.name]}
      hidden_item ->
        %{description: item_descriptions["hidden"][hidden_item.name]}
      actual_item ->
        actual_item.item
      true ->
        nil
    end
  end

  def get_exit(room, direction) do
    room
    |> Map.get(:exits)
    |> Enum.find(&(&1["direction"] == direction(direction)))
  end

  def mirror_exit(%Room{} = room, destination_id) do
    room
    |> Map.get(:exits)
    |> Enum.find(fn(%{"destination" => destination, "kind" => kind}) ->
         destination == destination_id and kind != "RemoteAction"
       end)
  end

  def command_exit(%Room{} = room, string) do
    room
    |> Map.get(:exits)
    |> Enum.find(fn(ex) ->
         ex["kind"] == "Command" and Enum.member?(ex["commands"], string)
       end)
  end

  def remote_action_exit(%Room{} = room, string) do
    room
    |> Map.get(:exits)
    |> Enum.find(fn(ex) ->
         ex["kind"] == "RemoteAction" and Enum.member?(ex["commands"], string)
       end)
  end

  def command(%Room{} = room, string) do
    command =
      room
      |> Map.get(:commands, %{})
      |> Map.keys
      |> Enum.find(fn(command) ->
           String.downcase(command) == String.downcase(string)
         end)

    if command do
      room.commands[command]
    end
  end

  def unlocked?(%Room{effects: effects}, direction) do
    effects
    |> Map.values
    |> Enum.filter(fn(effect) ->
         Map.has_key?(effect, :unlocked)
       end)
    |> Enum.map(fn(effect) ->
         Map.get(effect, :unlocked)
       end)
    |> Enum.member?(direction)
  end

  def temporarily_open?(%Room{} = room, direction) do
    room
    |> Map.get(:effects)
    |> Map.values
    |> Enum.filter(fn(effect) ->
         Map.has_key?(effect, :open)
       end)
    |> Enum.map(fn(effect) ->
         Map.get(effect, :open)
       end)
    |> Enum.member?(direction)
  end

  def searched?(room, direction) do
    room
    |> Map.get(:effects)
    |> Map.values
    |> Enum.filter(fn(effect) ->
         Map.has_key?(effect, :searched)
       end)
    |> Enum.map(fn(effect) ->
         Map.get(effect, :searched)
       end)
    |> Enum.member?(direction)
  end

  def exit_direction("up"),      do: "upwards"
  def exit_direction("down"),    do: "downwards"
  def exit_direction(direction), do: "to the #{direction}"

  def enter_direction(nil),       do: "nowhere"
  def enter_direction("up"),      do: "above"
  def enter_direction("down"),    do: "below"
  def enter_direction(direction), do: "the #{direction}"

  def send_scroll(id, html) when is_integer(id) do
    ApathyDrive.PubSub.broadcast! "rooms:#{id}:mobiles", {:scroll, html}
  end
  def send_scroll(%Room{id: id}, html) do
    ApathyDrive.PubSub.broadcast! "rooms:#{id}:mobiles", {:scroll, html}
  end

  def open!(%Room{} = room, direction) do
    if open_duration = get_exit(room, direction)["open_duration_in_seconds"] do
      Systems.Effect.add(room, %{open: direction}, open_duration)
    else
      exits = room.exits
              |> Enum.map(fn(room_exit) ->
                   if room_exit["direction"] == direction do
                     Map.put(room_exit, "open", true)
                   else
                     room_exit
                   end
                 end)
      Map.put(room, :exits, exits)
    end
  end

  def close!(%Room{effects: effects} = room, direction) do
    room = effects
           |> Map.keys
           |> Enum.filter(fn(key) ->
                effects[key][:open] == direction
              end)
           |> Enum.reduce(room, fn(room, key) ->
                Systems.Effect.remove(room, key, show_expiration_message: true)
              end)

    exits = room.exits
            |> Enum.map(fn(room_exit) ->
                 if room_exit["direction"] == direction do
                   Map.delete(room_exit, "open")
                 else
                   room_exit
                 end
               end)

    room = Map.put(room, :exits, exits)

    unlock!(room, direction)
  end

  def lock!(%Room{effects: effects} = room, direction) do
    effects
    |> Map.keys
    |> Enum.filter(fn(key) ->
         effects[key][:unlocked] == direction
       end)
    |> Enum.reduce(room, fn(key, room) ->
         Systems.Effect.remove(room, key, show_expiration_message: true)
       end)
  end

  def get_direction_by_destination(%Room{exits: exits}, destination_id) do
    exit_to_destination = exits
                          |> Enum.find(fn(room_exit) ->
                               room_exit["destination"] == destination_id
                             end)
    exit_to_destination && exit_to_destination["direction"]
  end

  defp unlock!(%Room{} = room, direction) do
    unlock_duration = if open_duration = get_exit(room, direction)["open_duration_in_seconds"] do
      open_duration
    else
      10#300
    end

    Systems.Effect.add(room, %{unlocked: direction}, unlock_duration)
    # todo: tell players in the room when it re-locks
    #"The #{name} #{ApathyDrive.Exit.direction_description(exit["direction"])} just locked!"
  end

  def direction(direction) do
    case direction do
      "n" ->
        "north"
      "ne" ->
        "northeast"
      "e" ->
        "east"
      "se" ->
        "southeast"
      "s" ->
        "south"
      "sw" ->
        "southwest"
      "w" ->
        "west"
      "nw" ->
        "northwest"
      "u" ->
        "up"
      "d" ->
        "down"
      direction ->
        direction
    end
  end

end
