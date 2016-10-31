defmodule ApathyDrive.Spell do
  use ApathyDrive.Web, :model
  alias ApathyDrive.{Match, Mobile, Room, Spell}

  schema "spells" do
    field :name, :string
    field :targets, :string
    field :kind, :string
    field :mana, :integer
    field :command, :string
    field :description, :string
    field :user_message, :string
    field :target_message, :string
    field :spectator_message, :string
    field :duration_in_ms, :integer
    field :cooldown_in_ms, :integer

    field :level, :integer, virtual: true
    field :abilities, :map, virtual: true, default: %{}
    field :ignores_round_cooldown?, :boolean, virtual: true, default: false

    has_many :characters, ApathyDrive.Character

    has_many :spells_abilities, ApathyDrive.SpellAbility

    timestamps
  end

  @required_fields ~w(name targets kind mana command description user_message target_message spectator_message duration_in_ms)
  @optional_fields ~w()

  @valid_targets ["monster or single", "self", "self or single", "monster", "full party area", "full attack area", "single", "full area"]
  @target_required_targets ["monster or single", "monster", "single"]

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(model, params \\ %{}) do
    model
    |> cast(params, @required_fields, @optional_fields)
  end

  def base_mana_at_level(level), do: 500 + ((level - 1) * 50)

  def mana_cost_at_level(%Spell{mana: mana} = spell, level) do
    trunc(base_mana_at_level(level) * (mana / 100))
  end

  def execute(%Room{} = room, caster_ref, %Spell{targets: targets}, "") when targets in @target_required_targets do
    room
    |> Room.get_mobile(caster_ref)
    |> Mobile.send_scroll("<p><span class='red'>You must specify a target for that spell.</span></p>")

    room
  end

  def execute(%Room{} = room, caster_ref, %Spell{} = spell, "") do
    execute(room, caster_ref, spell, List.wrap(caster_ref))
  end

  def execute(%Room{} = room, caster_ref, %Spell{} = spell, query) when is_binary(query) do
    if can_execute?(room, caster_ref, spell) do
      case get_targets(room, caster_ref, spell, query) do
        [] ->
          room
          |> Room.get_mobile(caster_ref)
          |> Mobile.send_scroll("<p>Unable to cast #{spell.name} at \"#{query}\".</p>")
        targets ->
          execute(room, caster_ref, spell, targets)
        end
    else
      room
    end
  end

  def execute(room, mobile_ref, %Spell{} = spell, targets) when is_list(targets) do
    Room.update_mobile(room, mobile_ref, fn(mobile) ->
      Mobile.send_scroll mobile, "<p>Casting #{spell.name}!</p>"
    end)
  end

  def can_execute?(%Room{} = room, caster_ref, spell) do
    mobile = Room.get_mobile(room, caster_ref)

    cond do
      on_cooldown?(mobile, spell) ->
        Mobile.send_scroll(mobile, "<p><span class='dark-cyan'>You can't do that yet.</p>")
        false
      Mobile.confused(mobile, room) ->
        false
      Mobile.silenced(mobile, room) ->
        false
      not_enough_mana?(mobile, spell) ->
        false
      true ->
        true
    end
  end

  def on_cooldown?(%{} = mobile, %Spell{cooldown_in_ms: nil} = spell) do
    on_round_cooldown?(mobile, spell)
  end
  def on_cooldown?(%{effects: effects} = mobile, %Spell{name: name} = ability) do
    effects
    |> Map.values
    |> Enum.any?(&(&1["cooldown"] == name)) or on_round_cooldown?(mobile, ability)
  end

  def on_round_cooldown?(_mobile, %{ignores_round_cooldown?: true}), do: false
  def on_round_cooldown?(mobile, %{}), do: on_round_cooldown?(mobile)
  def on_round_cooldown?(%{effects: effects}) do
    effects
    |> Map.values
    |> Enum.any?(&(&1["cooldown"] == :global))
  end

  def get_targets(%Room{} = room, caster_ref, %Spell{targets: "monster or single"}, query) do
    match =
      room.mobiles
      |> Map.values
      |> Enum.reject(& &1.ref == caster_ref)
      |> Match.one(:name_contains, query)

    List.wrap(match && match.ref)
  end
  def get_targets(%Room{} = room, caster_ref, %Spell{targets: "self"}, _query) do
    List.wrap(caster_ref)
  end
  def get_targets(%Room{} = room, caster_ref, %Spell{targets: "monster"}, query) do
    match =
      room.mobiles
      |> Map.values
      |> Enum.filter(& &1.__struct__ == Monster)
      |> Match.one(:name_contains, query)

    List.wrap(match && match.ref)
  end
  def get_targets(%Room{} = room, caster_ref, %Spell{targets: "full party area"}, _query) do
    caster_ref
    |> Room.get_mobile
    |> Mobile.party_refs(room)
  end
  def get_targets(%Room{} = room, caster_ref, %Spell{targets: "full attack area"}, _query) do
    party =
      caster_ref
      |> Room.get_mobile
      |> Mobile.party_refs(room)

    room.mobiles
    |> Map.values
    |> Kernel.--(party)
  end
  def get_targets(%Room{} = room, caster_ref, %Spell{targets: "self or single"}, query) do
    match =
      room.mobiles
      |> Map.values
      |> Enum.reject(& &1.__struct__ == Monster)
      |> Match.one(:name_contains, query)

    List.wrap(match && match.ref)
  end
  def get_targets(%Room{} = room, caster_ref, %Spell{targets: "single"}, query) do
    match =
      room.mobiles
      |> Map.values
      |> Enum.reject(& &1.__struct__ == Monster || &1.ref == caster_ref)
      |> Match.one(:name_contains, query)

    List.wrap(match && match.ref)
  end

  def not_enough_mana?(%{} = mobile, %Spell{ignores_round_cooldown?: true}), do: false
  def not_enough_mana?(%{} = mobile, %Spell{} = spell) do
    if !Mobile.enough_mana_for_spell?(mobile, spell) do
      Mobile.send_scroll(mobile, "<p><span class='red'>You do not have enough mana to use that ability.</span></p>")
    end
  end

end
