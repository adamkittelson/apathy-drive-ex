defmodule ApathyDrive.Room do
  use ApathyDriveWeb, :model

  alias ApathyDrive.{
    Ability,
    Area,
    Character,
    Companion,
    Match,
    Mobile,
    Monster,
    MonsterSpawning,
    Room,
    RoomServer,
    PubSub,
    TimerManager
  }

  require Logger

  @behaviour Access
  defdelegate get_and_update(container, key, fun), to: Map
  defdelegate fetch(container, key), to: Map
  defdelegate get(container, key, default), to: Map
  defdelegate pop(container, key), to: Map

  schema "rooms" do
    field(:name, :string)
    field(:description, :string)
    field(:light, :integer)
    field(:item_descriptions, ApathyDrive.JSONB, default: %{"hidden" => %{}, "visible" => %{}})
    field(:lair_size, :integer)
    field(:lair_frequency, :integer, default: 5)
    field(:exits, ApathyDrive.JSONB, default: [])
    field(:commands, ApathyDrive.JSONB, default: %{})
    field(:legacy_id, :string)
    field(:coordinates, ApathyDrive.JSONB)
    field(:permanent_npc, :integer)

    field(:effects, :map, virtual: true, default: %{})
    field(:lair_next_spawn_at, :integer, virtual: true, default: 0)
    field(:timers, :map, virtual: true, default: %{})
    field(:last_effect_key, :integer, virtual: true, default: 0)
    field(:mobiles, :map, virtual: true, default: %{})
    field(:timer, :any, virtual: true)
    field(:items, :any, virtual: true, default: [])
    field(:allies, :any, virtual: true, default: %{})
    field(:enemies, :any, virtual: true, default: %{})

    timestamps()

    has_many(:persisted_mobiles, Monster)
    belongs_to(:ability, Ability)
    belongs_to(:area, ApathyDrive.Area)
    has_many(:shop_items, ApathyDrive.ShopItem)
    has_many(:items_for_sales, through: [:shop_items, :item])
    has_many(:lairs, ApathyDrive.LairMonster)
    has_many(:lair_monsters, through: [:lairs, :monster])
    has_many(:room_skills, ApathyDrive.RoomSkill)
    has_many(:skills, through: [:room_skills, :skill])
  end

  def load_items(%Room{} = room) do
    items = ApathyDrive.ItemInstance.load_items(room)
    Map.put(room, :items, items)
  end

  def load_reputations(%Room{area: area} = room) do
    area =
      area
      |> Repo.preload(:allies)
      |> Repo.preload(:enemies)

    room
    |> put_in([:allies], Enum.reduce(area.allies, %{}, &Map.put(&2, &1.id, &1.name)))
    |> put_in([:enemies], Enum.reduce(area.enemies, %{}, &Map.put(&2, &1.id, &1.name)))
  end

  def load_skills(%Room{} = room) do
    Repo.preload(room, :skills, force: true)
  end

  def load_abilities(%Room{} = room) do
    Enum.reduce(room.mobiles, room, fn
      {ref, %Character{}}, updated_room ->
        Room.update_mobile(updated_room, ref, fn character ->
          Character.load_abilities(character)
        end)

      _, updated_room ->
        updated_room
    end)
  end

  def trainer?(%Room{} = room), do: Enum.any?(room.skills)

  def update_mobile(%Room{} = room, mobile_ref, fun) do
    if mobile = room.mobiles[mobile_ref] do
      case fun.(mobile) do
        %Room{} = updated_room ->
          updated_room

        %{} = updated_mobile ->
          put_in(room.mobiles[mobile_ref], updated_mobile)
      end
    else
      room
    end
  end

  def items_for_sale(%Room{} = room) do
    room
    |> assoc(:items_for_sales)
    |> Repo.all()
  end

  def mobile_entered(%Room{} = room, %kind{} = mobile, message \\ nil) do
    from_direction =
      room
      |> Room.get_direction_by_destination(mobile.room_id)
      |> Room.enter_direction()

    Room.display_enter_message(room, mobile, message)

    Room.audible_movement(room, from_direction)

    mobile =
      mobile
      |> Mobile.set_room_id(room.id)

    room = put_in(room.mobiles[mobile.ref], mobile)

    if kind == Character, do: ApathyDrive.Commands.Look.execute(room, mobile, [])

    room =
      if Map.has_key?(mobile, :leader) and is_nil(room.mobiles[mobile.leader]) do
        put_in(room.mobiles[mobile.ref], put_in(mobile.leader, mobile.ref))
      else
        room
      end

    room
    |> MonsterSpawning.spawn_permanent_npc()
    |> Room.move_after(mobile.ref)
    |> Room.start_timer()
  end

  def move_after(%Room{} = room, ref) do
    Room.update_mobile(room, ref, fn
      %Monster{} = monster ->
        monster

      # TimerManager.send_after(monster, {:monster_movement, jitter(:timer.seconds(frequency)), {:auto_move, ref}})
      %Character{} = character ->
        character

      %Companion{} = companion ->
        companion
    end)
  end

  def local_hated_targets(%Room{mobiles: mobiles}, %{hate: hate}) do
    mobiles
    |> Map.keys()
    |> Enum.reduce(%{}, fn potential_target, targets ->
      threat = Map.get(hate, potential_target, 0)

      if threat > 0 do
        Map.put(targets, threat, potential_target)
      else
        targets
      end
    end)
  end

  def next_timer(%Room{} = room) do
    [
      TimerManager.next_timer(room)
      | Enum.map(Map.values(room.mobiles), &TimerManager.next_timer/1)
    ]
    |> Enum.reject(&is_nil/1)
    |> Enum.sort()
    |> List.first()
  end

  def apply_timers(%Room{} = room) do
    room = TimerManager.apply_timers(room)

    Enum.reduce(room.mobiles, room, fn {ref, _mobile}, updated_room ->
      TimerManager.apply_timers(updated_room, ref)
    end)
  end

  def start_timer(%Room{timer: timer} = room) do
    if next_timer = Room.next_timer(room) do
      send_at = max(0, trunc(next_timer - DateTime.to_unix(DateTime.utc_now(), :millisecond)))

      cond do
        is_nil(timer) ->
          timer = Process.send_after(self(), :tick, send_at)
          Map.put(room, :timer, timer)

        Process.read_timer(timer) >= send_at ->
          Process.cancel_timer(timer)
          timer = Process.send_after(self(), :tick, send_at)
          Map.put(room, :timer, timer)

        :else ->
          room
      end
    else
      room
    end
  end

  def find_character(%Room{mobiles: mobiles}, character_id) do
    mobiles
    |> Map.values()
    |> Enum.find(fn
      %Character{id: ^character_id} ->
        true

      _ ->
        false
    end)
  end

  def find_monitor_ref(%Room{mobiles: mobiles}, ref) do
    mobiles
    |> Map.values()
    |> Enum.find(&(Map.get(&1, :monitor_ref) == ref))
  end

  def display_enter_message(%Room{} = room, %{} = mobile, message \\ nil) do
    from_direction =
      room
      |> get_direction_by_destination(mobile.room_id)
      |> enter_direction()

    room.mobiles
    |> Map.values()
    |> List.delete(mobile)
    |> Enum.each(fn
      %Character{} = observer ->
        message =
          (message || Mobile.enter_message(mobile))
          |> ApathyDrive.Text.interpolate(%{
            "name" => Mobile.colored_name(mobile, observer),
            "direction" => from_direction
          })
          |> ApathyDrive.Text.capitalize_first()

        Mobile.send_scroll(observer, "<p>#{message}</p>")

      _ ->
        :noop
    end)
  end

  def display_exit_message(room, %{mobile: mobile, message: message, to: to_room_id}) do
    room.mobiles
    |> Map.values()
    |> List.delete(mobile)
    |> Enum.each(fn
      %Character{} = observer ->
        message =
          message
          |> ApathyDrive.Text.interpolate(%{
            "name" => Mobile.colored_name(mobile, observer),
            "direction" =>
              room |> Room.get_direction_by_destination(to_room_id) |> Room.exit_direction()
          })

        Mobile.send_scroll(observer, "<p>#{message}</p>")

      _ ->
        :noop
    end)
  end

  def audible_movement(%Room{exits: exits}, from_direction) do
    exits
    |> Enum.each(fn
      %{"direction" => direction, "kind" => kind, "destination" => dest}
      when kind in ["Normal", "Action", "Door", "Gate", "Trap", "Cast"] and
             direction != from_direction ->
        direction = ApathyDrive.Exit.reverse_direction(direction)

        dest
        |> RoomServer.find()
        |> RoomServer.send_scroll(
          "<p><span class='dark-magenta'>You hear movement #{sound_direction(direction)}.</span></p>"
        )

      _ ->
        :noop
    end)
  end

  def initiate_remote_action(room, mobile, remote_action_exit, opts \\ []) do
    unless Mobile.confused(mobile, room) do
      remote_action_exit["destination"]
      |> RoomServer.find()
      |> RoomServer.trigger_remote_action(remote_action_exit, mobile.room_id, opts)

      Mobile.send_scroll(mobile, "<p>#{remote_action_exit["message"]}</p>")

      room.mobiles
      |> Map.values()
      |> List.delete(mobile)
      |> Enum.each(fn
        %Character{} = observer ->
          Mobile.send_scroll(
            observer,
            "<p>#{
              ApathyDrive.Text.interpolate(remote_action_exit["room_message"], %{
                "name" => Mobile.colored_name(mobile, observer)
              })
            }</span></p>"
          )

        _ ->
          :noop
      end)
    end

    room
  end

  def world_map do
    from(
      area in Area,
      select: %{id: area.id, level: area.level}
    )
  end

  def area_map(area_id) do
    from(
      room in Room,
      where: room.area_id == ^area_id and not is_nil(room.coordinates),
      join: area in assoc(room, :area),
      select: %{
        id: room.id,
        name: room.name,
        coords: room.coordinates,
        area: area.name,
        exits: room.exits
      }
    )
  end

  def update_area(%Room{area: %Area{name: old_area}} = room, %Area{} = area) do
    PubSub.unsubscribe("areas:#{room.area_id}")

    room =
      room
      |> Map.put(:area, area)
      |> Map.put(:area_id, area.id)
      |> Repo.save!()

    PubSub.subscribe("areas:#{area.id}")

    ApathyDriveWeb.Endpoint.broadcast!("map", "area_change", %{
      room_id: room.id,
      old_area: old_area,
      new_area: area.name
    })

    room
  end

  def changeset(%Room{} = room, params \\ %{}) do
    room
    |> cast(
      params,
      ~w(name description exits),
      ~w(light item_descriptions lair_size lair_frequency commands legacy_id coordinates)
    )
    |> validate_format(:name, ~r/^[a-zA-Z ,]+$/)
    |> validate_length(:name, min: 1, max: 30)
  end

  def find_mobile_in_room(%Room{mobiles: mobiles}, mobile, query) do
    mobiles =
      mobiles
      |> Map.values()

    mobiles
    |> Enum.reject(&(&1.ref == mobile.ref))
    |> List.insert_at(-1, mobile)
    |> Enum.reject(&(&1 == nil))
    |> Match.one(:name_contains, query)
  end

  def datalist do
    __MODULE__
    |> Repo.all()
    |> Enum.map(fn mt ->
      "#{mt.name} - #{mt.id}"
    end)
  end

  def light(%Room{} = room) do
    light_source_average =
      room.mobiles
      |> Enum.map(fn {_ref, mobile} ->
        Mobile.ability_value(mobile, "Light")
      end)
      |> Enum.reject(&(&1 == 0))
      |> average()

    if light_source_average do
      trunc(light_source_average)
    else
      room.light
    end
  end

  def start_room_id do
    ApathyDrive.Config.get(:start_room)
  end

  def find_item(%Room{items: items, item_descriptions: item_descriptions}, item) do
    actual_item =
      items
      |> Enum.map(&%{name: &1.name, keywords: String.split(&1.name), item: &1})
      |> Match.one(:keyword_starts_with, item)

    visible_item =
      item_descriptions["visible"]
      |> Map.keys()
      |> Enum.map(&%{name: &1, keywords: String.split(&1)})
      |> Match.one(:keyword_starts_with, item)

    hidden_item =
      item_descriptions["hidden"]
      |> Map.keys()
      |> Enum.map(&%{name: &1, keywords: String.split(&1)})
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
    |> Enum.find(fn %{"destination" => destination, "kind" => kind} ->
      destination == destination_id and kind != "RemoteAction"
    end)
  end

  def command_exit(%Room{} = room, string) do
    room
    |> Map.get(:exits)
    |> Enum.find(fn ex ->
      ex["kind"] == "Command" and Enum.member?(ex["commands"], string)
    end)
  end

  def remote_action_exit(%Room{} = room, string) do
    room
    |> Map.get(:exits)
    |> Enum.find(fn ex ->
      ex["kind"] == "RemoteAction" and Enum.member?(ex["commands"], string)
    end)
  end

  def command(%Room{} = room, string) do
    command =
      room
      |> Map.get(:commands, %{})
      |> Map.keys()
      |> Enum.find(fn command ->
        String.downcase(command) == String.downcase(string)
      end)

    if command do
      room.commands[command]
    end
  end

  def unlocked?(%Room{effects: effects}, direction) do
    effects
    |> Map.values()
    |> Enum.filter(fn effect ->
      Map.has_key?(effect, :unlocked)
    end)
    |> Enum.map(fn effect ->
      Map.get(effect, :unlocked)
    end)
    |> Enum.member?(direction)
  end

  def temporarily_open?(%Room{} = room, direction) do
    room
    |> Map.get(:effects)
    |> Map.values()
    |> Enum.filter(fn effect ->
      Map.has_key?(effect, :open)
    end)
    |> Enum.map(fn effect ->
      Map.get(effect, :open)
    end)
    |> Enum.member?(direction)
  end

  def searched?(room, direction) do
    room
    |> Map.get(:effects)
    |> Map.values()
    |> Enum.filter(fn effect ->
      Map.has_key?(effect, :searched)
    end)
    |> Enum.map(fn effect ->
      Map.get(effect, :searched)
    end)
    |> Enum.member?(direction)
  end

  def exit_direction("up"), do: "upwards"
  def exit_direction("down"), do: "downwards"
  def exit_direction(direction), do: "to the #{direction}"

  def enter_direction(nil), do: "nowhere"
  def enter_direction("up"), do: "above"
  def enter_direction("down"), do: "below"
  def enter_direction(direction), do: "the #{direction}"

  def send_scroll(%Room{mobiles: mobiles} = room, html, exclude_mobiles \\ []) do
    refs_to_exclude = Enum.map(exclude_mobiles, & &1.ref)

    mobiles
    |> Map.values()
    |> Enum.each(fn %{ref: ref} = mobile ->
      if !(ref in refs_to_exclude), do: Mobile.send_scroll(mobile, html)
    end)

    room
  end

  def open!(%Room{} = room, direction) do
    if open_duration = get_exit(room, direction)["open_duration_in_seconds"] do
      Systems.Effect.add(room, %{open: direction}, open_duration)
    else
      exits =
        room.exits
        |> Enum.map(fn room_exit ->
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
    room =
      effects
      |> Map.keys()
      |> Enum.filter(fn key ->
        effects[key][:open] == direction
      end)
      |> Enum.reduce(room, fn room, key ->
        Systems.Effect.remove(room, key, show_expiration_message: true)
      end)

    exits =
      room.exits
      |> Enum.map(fn room_exit ->
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
    |> Map.keys()
    |> Enum.filter(fn key ->
      effects[key][:unlocked] == direction
    end)
    |> Enum.reduce(room, fn key, room ->
      Systems.Effect.remove(room, key, show_expiration_message: true)
    end)
  end

  def get_direction_by_destination(%Room{exits: exits}, destination_id) do
    exit_to_destination =
      exits
      |> Enum.find(fn room_exit ->
        room_exit["destination"] == destination_id
      end)

    exit_to_destination && exit_to_destination["direction"]
  end

  defp unlock!(%Room{} = room, direction) do
    unlock_duration =
      if open_duration = get_exit(room, direction)["open_duration_in_seconds"] do
        open_duration
      else
        # 300
        10
      end

    Systems.Effect.add(room, %{unlocked: direction}, unlock_duration)
    # todo: tell players in the room when it re-locks
    # "The #{name} #{ApathyDrive.Exit.direction_description(exit["direction"])} just locked!"
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

  def average([]), do: nil

  def average(list) do
    Enum.sum(list) / length(list)
  end

  defp sound_direction("up"), do: "above you"
  defp sound_direction("down"), do: "below you"
  defp sound_direction(direction), do: "to the #{direction}"
end
