defmodule ApathyDrive.Room do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Ability, Area, Match, Mobile, Room, RoomServer, RoomUnity, Presence, PubSub, TimerManager}

  schema "rooms" do
    field :name,                     :string
    field :description,              :string
    field :light,                    :integer
    field :item_descriptions,        ApathyDrive.JSONB, default: %{"hidden" => %{}, "visible" => %{}}
    field :lair_size,                :integer
    field :lair_frequency,           :integer, default: 5
    field :exits,                    ApathyDrive.JSONB, default: []
    field :commands,                 ApathyDrive.JSONB, default: %{}
    field :legacy_id,                :string
    field :coordinates,              ApathyDrive.JSONB

    field :effects,                  :map, virtual: true, default: %{}
    field :lair_next_spawn_at,       :integer, virtual: true, default: 0
    field :timers,                   :map, virtual: true, default: %{}
    field :last_effect_key,          :integer, virtual: true, default: 0
    field :default_essence,          :integer, virtual: true
    field :mobiles,                  :map, virtual: true, default: %{}
    field :timer,                    :any, virtual: true

    timestamps

    has_one    :room_unity, RoomUnity
    has_many   :persisted_mobiles, Mobile
    belongs_to :ability, Ability
    belongs_to :area, ApathyDrive.Area
    has_many   :lairs, ApathyDrive.LairMonster
    has_many   :lair_monsters, through: [:lairs, :monster_template]
  end

  def update_mobile(%Room{} = room, mobile_ref, fun) do
    if mobile = room.mobiles[mobile_ref] do
      put_in(room.mobiles[mobile_ref], fun.(mobile))
    else
      room
    end
  end

  def next_timer(%Room{} = room) do
    [TimerManager.next_timer(room) | Enum.map(Map.values(room.mobiles), &TimerManager.next_timer/1)]
    |> Enum.reject(&is_nil/1)
    |> Enum.sort
    |> List.first
  end

  def apply_timers(%Room{} = room) do
    room = TimerManager.apply_timers(room)

    Enum.reduce(room.mobiles, room, fn {ref, mobile}, updated_room ->
      put_in updated_room.mobiles[ref], TimerManager.apply_timers(mobile)
    end)
  end

  def start_timer(%Room{timer: timer} = room) do
    if next_timer = Room.next_timer(room) do
      send_at = max(0, trunc(next_timer - Timex.Time.to_milliseconds(Timex.Time.now)))
      cond do
        is_nil(timer) ->
          timer = Process.send_after(self, :tick, send_at)
          Map.put(room, :timer, timer)
        Process.read_timer(timer) >= send_at ->
          Process.cancel_timer(timer)
          timer = Process.send_after(self, :tick, send_at)
          Map.put(room, :timer, timer)
        :else ->
          room
      end
    else
      room
    end
  end

  def find_spirit(%Room{mobiles: mobiles}, spirit_id) do
    mobiles
    |> Map.values
    |> Enum.find(&(&1.spirit && &1.spirit.id == spirit_id))
  end

  def load_present_mobiles(%Room{} = room) do
    room.id
    |> mobiles_to_load()
    |> Enum.reduce(room, fn(mobile_id, updated_room) ->
         monster =
           Mobile
           |> Repo.get!(mobile_id)
           |> Mobile.init

         Room.audible_movement(room, nil)

         Room.display_enter_message(room, monster)

         put_in(updated_room.mobiles[monster.ref], monster)
       end)
  end

  def mobiles_to_load(room_id) do
    require Ecto.Query

    Mobile
    |> Ecto.Query.where(room_id: ^room_id)
    |> Ecto.Query.select([m], m.id)
    |> Repo.all
  end

  def display_enter_message(%Room{} = room, %Mobile{} = mobile, message \\ nil) do
    from_direction =
      room
      |> get_direction_by_destination(mobile.room_id)
      |> enter_direction()

    message =
      (message || mobile.enter_message)
      |> ApathyDrive.Text.interpolate(%{
           "name" => Mobile.look_name(mobile),
           "direction" => from_direction
         })
      |> ApathyDrive.Text.capitalize_first

    send_scroll(room, "<p>#{message}</p>", mobile)
  end

  def display_exit_message(room, %{mobile: mobile, message: message, to: to_room_id}) do
    message = message
              |> ApathyDrive.Text.interpolate(%{
                   "name" => Mobile.look_name(mobile),
                   "direction" => room |> Room.get_direction_by_destination(to_room_id) |> Room.exit_direction
                 })

    if mobile.monster_template_id do
      send_scroll(room, "<p><span class='grey'>#{message}</span></p>", mobile)
    else
      room
    end
  end

  def audible_movement(%Room{exits: exits}, from_direction) do
    exits
    |> Enum.each(fn
         %{"direction" => direction, "kind" => kind, "destination" => dest} when kind in ["Normal", "Action", "Door", "Gate", "Trap", "Cast"] and direction != from_direction ->
           dest
           |> RoomServer.find
           |> RoomServer.send_scroll("<p><span class='dark-magenta'>You hear movement #{sound_direction(direction)}.</span></p>")
         _ -> :noop
       end)
  end

  def initiate_remote_action(room, mobile, remote_action_exit, opts \\ []) do
    unless Mobile.confused(mobile) do
      remote_action_exit["destination"]
      |> RoomServer.find
      |> RoomServer.trigger_remote_action(remote_action_exit, mobile.room_id, opts)

      room
      |> Room.send_scroll(%{
           mobile.spirit.id => "<p>#{remote_action_exit["message"]}</p>",
           :other => "<p>#{ApathyDrive.Text.interpolate(remote_action_exit["room_message"], %{"name" => Mobile.look_name(mobile)})}</span></p>"
         })
    end
    room
  end

  def world_map do
    from room in Room,
    where: not is_nil(room.coordinates),
    join: area in assoc(room, :area),
    join: room_unity in assoc(room, :room_unity),
    select: %{id: room.id, name: room.name, coords: room.coordinates, area: area.name, controlled_by: room_unity.controlled_by, exits: room.exits}
  end

  def controlled_by(%Room{} = room) do
    room.room_unity.controlled_by
  end

  def update_area(%Room{area: %Area{name: old_area}} = room, %Area{} = area) do
    PubSub.unsubscribe("areas:#{room.area_id}")
    room =
      room
      |> Map.put(:area, area)
      |> Map.put(:area_id, area.id)
      |> set_default_essence()
      |> Repo.save!
    PubSub.subscribe("areas:#{area.id}")
    ApathyDrive.Endpoint.broadcast!("map", "area_change", %{room_id: room.id, old_area: old_area, new_area: area.name})
    room
  end

  def set_default_essence(%Room{room_unity: %RoomUnity{controlled_by: nil}} = room) do
    room = Map.put(room, :default_essence, default_essence(room))
    put_in(room.room_unity.essences["default"], room.default_essence)
  end
  def set_default_essence(%Room{} = room) do
    Map.put(room, :default_essence, default_essence(room))
  end

  def adjacent_room_data(%Room{} = room, data \\ %{}) do
    Map.merge(data, %{
      essences: room.room_unity.essences,
      room_id: room.id,
      area: room.area.name,
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
        highest_essence == "good" and essences["good"] > 0 and (essences["good"] * 0.9) > essences["evil"] ->
          "good"
        highest_essence == "evil" and essences["evil"] > 0 and (essences["evil"] * 0.9) > essences["good"] ->
          "evil"
        true ->
          nil
      end

    if controlled_by != new_controlled_by do
      ApathyDrive.Endpoint.broadcast!("map", "room control change", %{room_id: room.id, controlled_by: new_controlled_by})

      put_in(room.room_unity.controlled_by, new_controlled_by)
      |> Repo.save
    else
      room
    end
  end

  def default_essence(%Room{area: %Area{level: level}}) do
    ApathyDrive.Level.exp_at_level(level)
  end

  def changeset(%Room{} = room, params \\ %{}) do
    room
    |> cast(params, ~w(name description exits), ~w(light item_descriptions lair_size lair_frequency commands legacy_id coordinates))
    |> validate_format(:name, ~r/^[a-zA-Z ,]+$/)
    |> validate_length(:name, min: 1, max: 30)
  end

  def find_mobile_in_room(%Room{mobiles: mobiles}, mobile, query) do
    mobiles =
      mobiles
      |> Map.values

    mobiles
    |> Enum.reject(&(&1 == mobile))
    |> List.insert_at(-1, mobile)
    |> Enum.reject(&(&1 == nil))
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

  def find_item(%Room{room_unity: %RoomUnity{items: items}, item_descriptions: item_descriptions}, item) do
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

  def get_mobile(%Room{mobiles: mobiles}, ref) do
    mobiles[ref]
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

  def send_scroll(%Room{mobiles: mobiles}, html, exclude_mobile \\ nil) do
    mobiles
    |> Map.values
    |> Enum.each(fn mobile ->
         if mobile != exclude_mobile, do: Mobile.send_scroll(mobile, html)
       end)
  end

  def broadcast!(%Room{mobiles: mobiles}, message) do
    mobiles
    |> Map.values
    |> Enum.each(fn mobile ->
         if mobile.socket, do: send(mobile.socket, message)
       end)
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

  def report_essence(%Room{exits: exits, room_unity: %RoomUnity{essences: essences}} = room) do
    Enum.each(exits, fn(%{"destination" => dest, "direction" => direction, "kind" => kind}) ->
      unless kind in ["Cast", "RemoteAction"] do

        report = %{
          essences: essences,
          room_id: room.id,
          direction: direction,
          kind: kind,
          legacy_id: room.legacy_id,
          area: room.area.name,
          controlled_by: room.room_unity.controlled_by
        }

        dest
        |> RoomServer.find()
        |> send({:essence_report, report})
      end
    end)
  end

  def update_essence(%Room{room_unity: %RoomUnity{essences: essences}} = room) do
    room = if Enum.any?(room.room_unity.essence_targets), do: room, else: update_essence_targets(room)

    room =
      essences
      |> Enum.reduce(room, fn({essence, amount}, %Room{room_unity: %RoomUnity{essence_targets: essence_targets}} = updated_room) ->
           target = get_in(essence_targets, [essence, "target"])
           difference = target - amount
           amount_to_shift = difference / 10 / 60

           if abs(amount_to_shift) < 1 or difference == 0 or (amount > 0 and (1 - (abs(difference) / amount)) < 0.01) do
             put_in(updated_room.room_unity.essences[essence], target)
           else
             update_in(updated_room.room_unity.essences[essence], &(max(0, &1 + amount_to_shift)))
           end
         end)
      |> Room.update_controlled_by

    data =
      %{
        room_id: room.id,
        good: trunc(room.room_unity.essences["good"]),
        default: trunc(room.room_unity.essences["default"]),
        evil: trunc(room.room_unity.essences["evil"]),
      }

    broadcast!(room, {:update_room_essence, data})

    room
  end

  def update_essence_targets(%Room{room_unity: %RoomUnity{exits: exits, essences: current_essences, controlled_by: controlled_by}} = room) do
    initial_essences =
      %{"good"    => %{"adjacent" => [],
                       "mobile" => []},
        "evil"    => %{"adjacent" => [],
                       "mobile" => []},
        "default" => %{"adjacent" => [],
                       "mobile" => []}}

    area_exits =
      exits
      |> Enum.filter(fn({_direction, data}) ->
           data["area"] == room.area.name
         end)
      |> Enum.into(%{})

    {essences, control_amount} =
      cond do
        room.lair_next_spawn_at > 0 and room.default_essence > 0 and controlled_by == nil ->
          {put_in(initial_essences["default"]["control"], room.default_essence), room.default_essence}
        room.lair_next_spawn_at > 0 and room.default_essence > current_essences[controlled_by] ->
          {put_in(initial_essences[controlled_by]["control"], room.default_essence), room.default_essence}
        true ->
          {initial_essences, nil}
      end

    essences =
      area_exits
      |> Map.values
      |> Enum.map(&(&1["essences"]))
      |> Enum.reduce(essences, fn(exit_essences, adjacent_essences) ->
           adjacent_essences
           |> update_in(["good", "adjacent"],    &([exit_essences["good"] | &1]))
           |> update_in(["evil", "adjacent"],    &([exit_essences["evil"] | &1]))
           |> update_in(["default", "adjacent"], &([exit_essences["default"] | &1]))
         end)

    essences =
      room.mobiles
      |> Map.values
      |> Enum.map(&Mobile.track_data/1)
      |> Enum.reduce(essences, fn
           %{invisible?: nil}, updated_essences ->
             updated_essences
           %{spirit_essence: nil, unities: []} = mobile, updated_essences ->
             add_mobile_essence(updated_essences, ["default"], mobile.essence)
           %{spirit_essence: nil} = mobile, updated_essences ->
             add_mobile_essence(updated_essences, mobile.unities, mobile.essence)
           mobile, updated_essences ->
             add_mobile_essence(updated_essences, mobile.spirit_unities, mobile.spirit_essence)
         end)

    essences =
      essences
      |> Enum.reduce(essences, fn({unity, %{"adjacent" => adj, "mobile" => mobile}}, updated_essences) ->
           updated_essences
           |> put_in([unity, "adjacent"], average(adj))
           |> put_in([unity, "mobile"], average(mobile))
         end)

    essences =
      essences
      |> Enum.reduce(essences, fn
           {_unity, %{"control" => _control}}, updated_essences ->
             updated_essences
           {unity, _targets}, updated_essences ->
             if control_amount && updated_essences[unity]["adjacent"] do
               update_in(updated_essences, [unity, "adjacent"], &(max(0, &1 - control_amount)))
             else
               updated_essences
             end
         end)

    essences =
      essences
      |> Enum.reduce(essences, fn({unity, %{} = targets}, updated_essences) ->
           target =
             [targets["adjacent"], targets["mobile"], targets["control"] || nil]
             |> Enum.reject(&(&1 == nil))
             |> average()

           put_in(updated_essences[unity]["target"], target || 0)
         end)

    put_in(room.room_unity.essence_targets, essences)
  end

  def average([]), do: nil
  def average(list) do
    Enum.sum(list) / length(list)
  end

  defp add_mobile_essence(essences, mobile_unities, mobile_essence) do
    Enum.reduce(essences, essences, fn({unity, _list}, updated_essences) ->
      if unity in mobile_unities do
        updated_essences
        |> update_in([unity, "mobile"], &([mobile_essence | &1]))
      else
        updated_essences
      end
    end)
  end

  defp sound_direction("up"),      do: "above you"
  defp sound_direction("down"),    do: "below you"
  defp sound_direction(direction), do: "to the #{direction}"

end
