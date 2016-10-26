defmodule ApathyDrive.Character do
  use Ecto.Schema
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Character, CharacterItem, Item, Mobile, Room, Text}

  require Logger
  import Comeonin.Bcrypt

  schema "characters" do
    belongs_to :room, Room
    belongs_to :class, ApathyDrive.Class
    belongs_to :race, ApathyDrive.Race

    field :name,        :string
    field :gender,      :string
    field :email,       :string
    field :password,    :string
    field :external_id, :string
    field :experience,  :integer, default: 0
    field :level,       :integer, default: 1
    field :timers,      :map, virtual: true, default: %{}
    field :admin,       :boolean
    field :flags,       :map, default: %{}
    field :monitor_ref, :any, virtual: true
    field :ref,         :any, virtual: true
    field :socket,      :any, virtual: true
    field :effects,     :map, virtual: true, default: %{}
    field :hp,          :float, virtual: true, default: 1.0
    field :mana,        :float, virtual: true, default: 1.0
    field :gold,        :integer, default: 0

    has_many :characters_items, ApathyDrive.CharacterItem
    has_many :items, through: [:characters_items, :item]

    timestamps
  end

  @doc """
  Creates a changeset based on the `model` and `params`.

  If `params` are nil, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(character, params \\ %{}) do
    character
    |> cast(params, ~w(name race_id class_id gender), ~w())
    |> validate_inclusion(:class_id, ApathyDrive.Class.ids)
    |> validate_inclusion(:race_id, ApathyDrive.Race.ids)
    |> validate_inclusion(:gender, ["male", "female"])
    |> validate_format(:name, ~r/^[a-zA-Z]+$/)
    |> unique_constraint(:name, name: :characters_lower_name_index, on: Repo)
    |> validate_length(:name, min: 1, max: 12)
  end

  def sign_up_changeset(character, params \\ %{}) do
    character
    |> cast(params, ~w(email password), [])
    |> validate_format(:email, ~r/@/)
    |> validate_length(:email, min: 3, max: 100)
    |> validate_length(:password, min: 6)
    |> unique_constraint(:email, name: :characters_lower_email_index, on: Repo)
    |> validate_confirmation(:password)
  end

  def inventory(%Character{characters_items: items} = character) do
    Enum.filter(items, &(&1.equipped == false))
  end

  def equipment(%Character{characters_items: items} = character) do
    Enum.filter(items, &(&1.equipped == true))
  end

  def sign_in(email, password) do
    player = Repo.get_by(Character, email: email)
    sign_in?(player, password) && player
  end

  def sign_in?(%Character{password: stored_hash}, password) do
    checkpw(password, stored_hash)
  end

  def sign_in?(nil, _password) do
    dummy_checkpw
  end

  def find_or_create_by_external_id(external_id) do
    case Repo.one from s in Character, where: s.external_id == ^external_id do
      %Character{} = character ->
        character
      nil ->
        %Character{room_id: Room.start_room_id, external_id: external_id}
        |> Repo.insert!
    end
  end

  def add_experience(%Character{level: level} = character, exp) do
    character =
      character
      |> Map.put(:experience, character.experience + exp)
      |> ApathyDrive.Level.advance

    if character.level > level do
      Mobile.send_scroll character, "<p>You ascend to level #{character.level}!"
    end

    if character.level < level do
      Mobile.send_scroll character, "<p>You fall to level #{character.level}!"
    end
    character
  end

  def update_prompt(%Character{socket: socket} = character) do
    send(socket, {:update_prompt, prompt(character)})
  end

  def prompt(%Character{level: level, hp: hp_percent, mana: mana_percent} = character) do
    max_hp = Mobile.max_hp_at_level(character, level)
    max_mana = Mobile.max_mana_at_level(character, level)
    hp = trunc(max_hp * hp_percent)
    mana = trunc(max_mana * mana_percent)

    cond do
      hp_percent > 0.5 ->
        "[HP=#{hp}/MA=#{mana}]:"
      hp_percent > 0.20 ->
        "[HP=<span class='dark-red'>#{hp}</span>/MA=#{mana}]:"
      true ->
        "[HP=<span class='red'>#{hp}</span>/MA=#{mana}}]:"
    end
  end

  def hp_at_level(%Character{} = character, level) do
    max_hp = Mobile.max_hp_at_level(character, level)

    trunc(max_hp * character.hp)
  end

  def mana_at_level(%Character{} = character, level) do
    max_mana = Mobile.max_mana_at_level(character, level)

    trunc(max_mana * character.mana)
  end

  def score_data(%Character{} = character) do
    effects =
      character.effects
      |> Map.values
      |> Enum.filter(&(Map.has_key?(&1, "effect_message")))
      |> Enum.map(&(&1["effect_message"]))

    %{
      name: character.name,
      class: character.class.name,
      race: character.race.name,
      level: character.level,
      experience: character.experience,
      hp: hp_at_level(character, character.level),
      max_hp: Mobile.max_hp_at_level(character, character.level),
      hp: mana_at_level(character, character.level),
      max_mana: Mobile.max_mana_at_level(character, character.level),
      strength: Mobile.attribute_at_level(character, :strength, character.level),
      agility: Mobile.attribute_at_level(character, :agility, character.level),
      intellect: Mobile.attribute_at_level(character, :intellect, character.level),
      willpower: Mobile.attribute_at_level(character, :willpower, character.level),
      health: Mobile.attribute_at_level(character, :health, character.level),
      charm: Mobile.attribute_at_level(character, :charm, character.level),
      effects: effects
    }
  end

  def add_item(%Character{} = character, %Item{} = item, level) do
    %CharacterItem{character_id: character.id, item_id: item.id, level: level}
    |> Repo.insert!

    Repo.preload(character, [characters_items: :item], [force: true])
  end

  defimpl ApathyDrive.Mobile, for: Character do
    def attribute_at_level(%Character{} = character, attribute, level) do
      base = Map.get(character.race, attribute)

      trunc(base + ((base / 10) * (level - 1)))
    end

    def confused(%Character{effects: effects} = character, %Room{} = room) do
      effects
      |> Map.values
      |> Enum.find(fn(effect) ->
           Map.has_key?(effect, "confused") && (effect["confused"] >= :rand.uniform(100))
         end)
      |> confused(character, room)
    end
    def confused(nil, %Character{}, %Room{}), do: false
    def confused(%{"confusion_message" => %{"user" => user_message} = message}, %Character{} = character, %Room{} = room) do
      Mobile.send_scroll(character, user_message)
      if message["spectator"], do: Room.send_scroll(room, "#{Text.interpolate(message["spectator"], %{"user" => character})}", character)
      true
    end
    def confused(%{}, %Character{} = character, %Room{} = room) do
      send_scroll(character, "<p><span class='cyan'>You fumble in confusion!</span></p>")
      Room.send_scroll(room, "<p><span class='cyan'>#{Text.interpolate("{{user}} fumbles in confusion!</span></p>", %{"user" => character})}</span></p>", character)
      true
    end

    def enter_message(%Character{name: name}) do
      "<p><span class='yellow'>#{name}</span><span class='green'> walks off {{direction}}.</span></p>"
    end

    def exit_message(%Character{name: name}) do
      "<p><span class='yellow'>#{name}</span><span class='green'> walks in from {{direction}}.</span></p>"
    end

    def look_name(%Character{name: name}) do
      "<span class='dark-cyan'>#{name}</span>"
    end

    def max_hp_at_level(mobile, level) do
      trunc(5 * attribute_at_level(mobile, :health, level))
    end

    def max_mana_at_level(mobile, level) do
      trunc(5 * attribute_at_level(mobile, :intellect, level))
    end

    def held(%{effects: effects} = mobile) do
      effects
      |> Map.values
      |> Enum.find(fn(effect) ->
           Map.has_key?(effect, "held")
         end)
      |> held(mobile)
    end
    def held(nil, %{}), do: false
    def held(%{"effect_message" => message}, %{} = mobile) do
      send_scroll(mobile, "<p>#{message}</p>")
      true
    end

    def send_scroll(%Character{socket: socket} = character, html) do
      send(socket, {:scroll, html})
      character
    end

    def set_room_id(%Character{socket: socket, monitor_ref: monitor_ref} = character, room_id) do
      Process.demonitor(monitor_ref)

      send(character.socket, {:update_room, room_id})

      character
      |> Map.put(:room_id, room_id)
      |> Map.put(:monitor_ref, Process.monitor(socket))
      |> Repo.save!
    end

  end

end
