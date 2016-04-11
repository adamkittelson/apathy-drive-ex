defmodule ApathyDrive.Mobile do
  alias ApathyDrive.{Commands, Mobile, Repo, Item, ItemDrop, PubSub, TimerManager, Ability, World, Match, Class}
  use GenServer
  use ApathyDrive.Web, :model
  import Systems.Text
  import TimerManager, only: [seconds: 1]

  schema "mobiles" do
    belongs_to :room, Room
    belongs_to :monster_template, MonsterTemplate

    field :name,                 :string
    field :alignment,            :string
    field :enter_message,        :string,           default: "{{name}} enters from {{direction}}."
    field :exit_message,         :string,           default: "{{name}} leaves {{direction}}."
    field :death_message,        :string,           default: "{{name}} dies."
    field :description,          :string,           default: "Some temporary description."
    field :gender,               :string
    field :greeting,             :string
    field :level,                :integer,          default: 1
    field :flags,                {:array, :string}, default: []
    field :experience,           :integer
    field :auto_attack_interval, :float,            default: 4.0
    field :questions,            ApathyDrive.JSONB
    field :unity,                :string
    field :movement,             :string
    field :spawned_at,           :integer

    field :spirit,             :any,     virtual: true
    field :socket,             :any,     virtual: true
    field :hp,                 :float,   virtual: true
    field :max_hp,             :integer, virtual: true
    field :strength,           :integer, virtual: true
    field :agility,            :integer, virtual: true
    field :will,               :integer, virtual: true
    field :mana,               :float,   virtual: true
    field :max_mana,           :integer, virtual: true
    field :effects,            :any,     virtual: true, default: %{}
    field :pid,                :any,     virtual: true
    field :keywords,           :any,     virtual: true, default: []
    field :abilities,          :any,     virtual: true, default: []
    field :hate,               :any,     virtual: true, default: %{}
    field :timers,             :any,     virtual: true, default: %{}
    field :attack_target,      :any,     virtual: true
    field :combo,              :any,     virtual: true
    field :delayed,            :boolean, virtual: true, default: false
    field :last_effect_key,    :any,     virtual: true, default: 0
    field :chance_to_follow,   :integer, virtual: true, default: 0
    field :movement_frequency, :integer, virtual: true, default: 60
    field :last_room,          :any,     virtual: true

    timestamps
  end

  def ids do
    __MODULE__
    |> distinct(true)
    |> select([m], %{id: m.id, room_id: m.room_id, monster_template_id: m.monster_template_id})
  end

  def start(%Mobile{} = mobile, opts \\ []) do
    GenServer.start(__MODULE__, mobile, opts)
  end

  def start_link(id, opts \\ []) do
    GenServer.start_link(__MODULE__, id, opts)
  end

  def say(mobile, message) do
    GenServer.cast(mobile, {:say, message})
  end

  def gossip(mobile, message) do
    GenServer.cast(mobile, {:gossip, message})
  end

  def ask(mobile, target, question) do
    GenServer.cast(mobile, {:ask, target, question})
  end

  def bash(mobile, arguments) do
    GenServer.cast(mobile, {:bash, arguments})
  end

  def execute_room_command(mobile, scripts) do
    GenServer.cast(mobile, {:execute_room_command, scripts})
  end

  def trigger_remote_action(mobile, remote_action_exit) do
    GenServer.cast(mobile, {:trigger_remote_action, remote_action_exit})
  end

  def move(mobile, room, room_exit, last_room) do
    GenServer.cast(mobile, {:move, room, room_exit, last_room})
  end

  def execute_command(mobile, command, arguments) do
    GenServer.cast(mobile, {:execute_command, command, arguments})
  end

  def use_ability(pid, command, arguments) do
    GenServer.cast(pid, {:use_ability, command, arguments})
  end

  def auto_move(pid, exit_and_last_room) do
    GenServer.cast(pid, {:auto_move, exit_and_last_room})
  end

  def greet(mobile, target) do
    GenServer.cast(mobile, {:greet, target})
  end

  def list_forms(mobile, slot \\ "all") do
    GenServer.cast(mobile, {:list_forms, slot})
  end

  def display_enter_message(mobile, room, direction \\ nil) do
    GenServer.cast(mobile, {:display_enter_message, room, direction})
  end

  def forms(%Mobile{spirit: nil}), do: nil
  def forms(%Mobile{spirit: spirit}) do
    spirit
    |> Ecto.Model.assoc(:recipe_items)
    |> ApathyDrive.Repo.all
  end

  def purify_room(mobile) do
    GenServer.cast(mobile, :purify_room)
  end

  def add_experience(mobile, exp) when is_pid(mobile) do
    GenServer.cast(mobile, {:add_experience, exp})
  end
  def add_experience(%Mobile{experience: experience, level: level} = mobile, exp) do
    mobile =
      mobile
      |> Map.put(:experience, experience + exp)
      |> ApathyDrive.Level.advance
      |> Map.put(:spirit, Spirit.add_experience(mobile.spirit, exp))

    if mobile.level > level do
      send_scroll mobile, "<p>You ascend to level #{mobile.level}!"
      ApathyDrive.Endpoint.broadcast!("#{mobile.unity}-unity:mobiles", "scroll", %{:html => "<p>[<span class='yellow'>unity</span>]: <span class='#{Mobile.alignment_color(mobile)}'>#{mobile.name}</span> ascends to level #{mobile.level}!</p>"})
    end

    if mobile.level < level do
      send_scroll mobile, "<p>You fall to level #{mobile.level}!"
      ApathyDrive.Endpoint.broadcast!("#{mobile.unity}-unity:mobiles", "scroll", %{:html => "<p>[<span class='yellow'>unity</span>]: <span class='#{Mobile.alignment_color(mobile)}'>#{mobile.name}</span> descends to level #{mobile.level}!</p>"})
    end

    mobile
  end

  def add_form(mobile, item) do
    GenServer.cast(mobile, {:add_form, item})
  end

  def execute_script(pid, script) do
    GenServer.cast(pid, {:execute_script, script})
  end

  def display_abilities(pid) do
    GenServer.cast(pid, :display_abilities)
  end

  def display_cooldowns(pid) do
    GenServer.cast(pid, :display_cooldowns)
  end

  def show_score(pid) do
    GenServer.cast(pid, :show_score)
  end

  def sanitize(message) do
    {:safe, message} = Phoenix.HTML.html_escape(message)

    message
  end

  def score_data(mobile) do
    effects =
      mobile.effects
      |> Map.values
      |> Enum.filter(&(Map.has_key?(&1, "effect_message")))
      |> Enum.map(&(&1["effect_message"]))

    %{name: mobile.spirit.name,
      class: (mobile.monster_template_id && mobile.name) || mobile.spirit.class.name,
      level: level(mobile),
      experience: mobile.spirit.experience,
      hp: mobile.hp,
      max_hp: mobile.max_hp,
      mana: mobile.mana,
      max_mana: mobile.max_mana,
      strength: strength(mobile),
      agility: agility(mobile),
      will: will(mobile),
      effects: effects,
      physical_defense: (1 - reduce_damage(mobile, "physical defense")) * 100,
      magical_defense:  (1 - reduce_damage(mobile, "magical defense")) * 100,
      physical_damage: physical_damage(mobile),
      magical_damage: magical_damage(mobile)}
  end

  def display_experience(pid) do
    GenServer.cast(pid, :display_experience)
  end

  def class_chat(pid, message) do
    GenServer.cast(pid, {:class_chat, message})
  end

  def aligned_spirit_name(%Mobile{spirit: %Spirit{name: name, class: %{alignment: "good"}}}) do
    "<span class='white'>#{name}</span>"
  end
  def aligned_spirit_name(%Mobile{spirit: %Spirit{name: name, class: %{alignment: "neutral"}}}) do
    "<span class='dark-cyan'>#{name}</span>"
  end
  def aligned_spirit_name(%Mobile{spirit: %Spirit{name: name, class: %{alignment: "evil"}}}) do
    "<span class='magenta'>#{name}</span>"
  end

  def effects(mobile) do
    mobile
    |> World.mobile
    |> Map.get(:effects)
  end

  def look(mobile, args \\ []) do
    GenServer.cast(mobile, {:look, args})
  end

  def look_at_mobile(mobile, looker) do
    GenServer.cast(mobile, {:look_at_mobile, looker})
  end

  def look_at_item(mobile, item) do
    GenServer.cast(mobile, {:look_at_item, item})
  end

  def update_prompt(%Mobile{socket: nil}), do: :noop

  def update_prompt(%Mobile{socket: socket} = mobile) do
    send(socket, {:update_prompt, prompt(mobile)})
  end

  def prompt(%Mobile{} = mobile) do
    "[HP=#{trunc(mobile.hp)}/MA=#{trunc(mobile.mana)}]:"
  end

  def alignment_color(%{alignment: "evil"}),    do: "magenta"
  def alignment_color(%{alignment: "good"}),    do: "white"
  def alignment_color(%{alignment: "neutral"}), do: "dark-cyan"

  def evil_points(%{alignment: "evil"}),    do: 250
  def evil_points(%{alignment: "good"}),    do: -215
  def evil_points(%{alignment: "neutral"}), do: 0

  def has_item?(%Mobile{spirit: %Spirit{inventory: inventory, equipment: equipment}}, item_template_id) do
    (inventory ++ equipment)
    |> Enum.map(&Map.get(&1, "id"))
    |> Enum.member?(item_template_id)
  end

  def remove_item?(%Mobile{spirit: %Spirit{inventory: inventory, equipment: equipment}} = mobile, item_template_id) do
    inventory_item =
      inventory
      |> Enum.find(&(&1["id"] == item_template_id))

    if inventory_item do
      put_in(mobile.spirit.inventory, List.delete(inventory, inventory_item))
    else
      equipment_item =
        equipment
        |> Enum.find(&(&1["id"] == item_template_id))

      if equipment_item do
        put_in(mobile.spirit.equipment, List.delete(equipment, equipment_item))
      end
    end
  end

  def get_item(mobile, item) do
    GenServer.cast(mobile, {:get_item, item})
  end

  def display_inventory(mobile) when is_pid(mobile) do
    GenServer.cast(mobile, :display_inventory)
  end

  def construct_item(mobile, item) do
    GenServer.cast(mobile, {:construct_item, item})
  end

  def absorb(mobile, item) do
    GenServer.cast(mobile, {:absorb, item})
  end

  def drop_item(mobile, item) do
    GenServer.cast(mobile, {:drop_item, item})
  end

  def equip_item(mobile, item) when is_pid(mobile) do
    GenServer.cast(mobile, {:equip_item, item})
  end

  def equip_item(%Mobile{spirit: %Spirit{inventory: inventory, equipment: equipment}} = mobile, %{"worn_on" => worn_on} = item) do
    cond do
      Enum.count(equipment, &(&1["worn_on"] == worn_on)) >= worn_on_max(item) ->
        item_to_remove =
          equipment
          |> Enum.find(&(&1["worn_on"] == worn_on))

        equipment =
          equipment
          |> List.delete(item_to_remove)
          |> List.insert_at(-1, item)

        inventory =
          inventory
          |> List.insert_at(-1, item_to_remove)
          |> List.delete(item)

          mobile = put_in(mobile.spirit.inventory, inventory)
          mobile = put_in(mobile.spirit.equipment, equipment)
                   |> set_abilities
                   |> set_max_mana
                   |> set_mana
                   |> set_max_hp
                   |> set_hp

        %{equipped: item, unequipped: [item_to_remove], mobile: mobile}
      conflicting_worn_on(worn_on) |> Enum.any? ->
        items_to_remove =
          equipment
          |> Enum.filter(&(&1["worn_on"] in conflicting_worn_on(worn_on)))

        equipment =
          equipment
          |> Enum.reject(&(&1 in items_to_remove))
          |> List.insert_at(-1, item)

        inventory =
          items_to_remove
          |> Enum.reduce(inventory, fn(item_to_remove, inv) ->
               List.insert_at(inv, -1, item_to_remove)
             end)
          |> List.delete(item)

          mobile = put_in(mobile.spirit.inventory, inventory)
          mobile = put_in(mobile.spirit.equipment, equipment)
                   |> set_abilities
                   |> set_max_mana
                   |> set_mana
                   |> set_max_hp
                   |> set_hp

        %{equipped: item, unequipped: items_to_remove, mobile: mobile}
      true ->
        equipment =
          equipment
          |> List.insert_at(-1, item)

        inventory =
          inventory
          |> List.delete(item)

        mobile = put_in(mobile.spirit.inventory, inventory)
        mobile = put_in(mobile.spirit.equipment, equipment)
                 |> set_abilities
                 |> set_max_mana
                 |> set_mana
                 |> set_max_hp
                 |> set_hp

        %{equipped: item, mobile: mobile}
    end
  end

  def unequip_item(mobile, item) do
    GenServer.call(mobile, {:unequip_item, item})
  end

  def max_encumbrance(%Mobile{} = mobile) do
    strength(mobile) * 48
  end

  def current_encumbrance(%Mobile{spirit: %Spirit{inventory: inventory, equipment: equipment}}) do
    (inventory ++ equipment)
    |> Enum.reduce(0, fn(item, encumbrance) ->
        encumbrance + item["weight"]
       end)
  end

  def remaining_encumbrance(%Mobile{} = mobile) do
    max_encumbrance(mobile) - current_encumbrance(mobile)
  end

  def held(%Mobile{effects: effects} = mobile) do
    effects
    |> Map.values
    |> Enum.find(fn(effect) ->
         Map.has_key?(effect, "held")
       end)
    |> held(mobile)
  end
  def held(nil, %Mobile{}), do: false
  def held(%{"effect_message" => message}, %Mobile{} = mobile) do
    send_scroll(mobile, "<p>#{message}</p>")
    true
  end

  def silenced(mobile) when is_pid(mobile) do
    mobile
    |> World.mobile
    |> silenced()
  end
  def silenced(%Mobile{effects: effects} = mobile, %{"mana_cost" => cost}) when cost > 0 do
    effects
    |> Map.values
    |> Enum.find(fn(effect) ->
         Map.has_key?(effect, "silenced")
       end)
    |> silenced(mobile)
  end
  def silenced(%Mobile{}, %{}), do: false
  def silenced(nil, %Mobile{}), do: false
  def silenced(%{"effect_message" => message}, %Mobile{} = mobile) do
    send_scroll(mobile, "<p>#{message}</p>")
  end

  def confused(%Mobile{effects: effects} = mobile) do
    effects
    |> Map.values
    |> Enum.find(fn(effect) ->
         Map.has_key?(effect, "confused") && (effect["confused"] >= :random.uniform(100))
       end)
    |> held(mobile)
  end
  def confused(nil, %Mobile{}), do: false
  def confused(%{"confusion_message" => %{"user" => user_message, "spectator" => spectator_message}}, %Mobile{} = mobile) do
    send_scroll(mobile, "<p>#{user_message}</p>")
    ApathyDrive.Endpoint.broadcast_from! self, "rooms:#{mobile.room_id}", "scroll", %{:html => "<p>#{interpolate(spectator_message, %{"user" => mobile})}</p>"}
    true
  end
  def confused(%{}, %Mobile{} = mobile) do
    send_scroll(mobile, "<p>You fumble in confusion!</p>")
    ApathyDrive.Endpoint.broadcast_from! self, "rooms:#{mobile.room_id}", "scroll", %{:html => "<p>#{interpolate("{{user}} fumbles in confusion!", %{"user" => mobile})}</p>"}
    true
  end

  def reduce_damage(%Mobile{} = mobile, "physical defense") do
    1 - (0.00044 * physical_defense(mobile))
  end
  def reduce_damage(%Mobile{} = mobile, "magical defense") do
    1 - (0.00044 * magical_defense(mobile))
  end
  def reduce_damage(%Mobile{} = mobile, mitigator) do
    1 - (0.01 * Mobile.effect_bonus(mobile, mitigator))
  end

  def reduce_damage(%Mobile{} = mobile, damage, nil), do: reduce_damage(mobile, damage, [])
  def reduce_damage(%Mobile{} = mobile, damage, mitigated_by) when is_list(mitigated_by) do
    multiplier = Enum.reduce(["damage resistance" | mitigated_by], 1, fn(mitigating_factor, multiplier) ->
      multiplier * reduce_damage(mobile, mitigating_factor)
    end)

    max(0, trunc(damage * multiplier))
  end

  def physical_defense(%Mobile{} = mobile) do
    (physical_damage(mobile) * 2) * (4 + 0.01 * level(mobile)) + effect_bonus(mobile, "physical defense")
  end

  def magical_defense(%Mobile{} = mobile) do
    (magical_damage(mobile) * 2) * (4 + 0.01 * level(mobile)) + effect_bonus(mobile, "magical defense")
  end

  def effect_bonus(%Mobile{effects: effects}, name) do
    effects
    |> Map.values
    |> Enum.map(fn
         (%{} = effect) ->
           Map.get(effect, name, 0)
         (_) ->
           0
       end)
    |> Enum.sum
  end

  def send_scroll(mobile, message) when is_pid(mobile) do
    send mobile, {:send_scroll, message}
  end
  def send_scroll(%Mobile{socket: nil} = mobile, _html),  do: mobile
  def send_scroll(%Mobile{socket: socket} = mobile, html) do
    send(socket, {:scroll, html})
    mobile
  end

  def init(id) when is_integer(id) do
    Repo.get!(Mobile, id)
    |> init()
  end
  def init(%Mobile{spirit: nil} = mobile) do
    mobile =
      mobile
      |> Map.put(:pid, self)
      |> set_abilities
      |> set_max_mana
      |> set_mana
      |> set_max_hp
      |> set_hp
      |> TimerManager.send_every({:monster_regen,    1_000, :regen})
      |> TimerManager.send_every({:periodic_effects, 3_000, :apply_periodic_effects})
      |> TimerManager.send_every({:monster_ai,       5_000, :think})
      |> TimerManager.send_every({:monster_present,  4_000, :notify_presence})
      |> move_after()

      ApathyDrive.PubSub.subscribe(self, "rooms:#{mobile.room_id}:mobiles")
      ApathyDrive.PubSub.subscribe(self, "rooms:#{mobile.room_id}:mobiles:#{mobile.alignment}")

      if mobile.monster_template_id do
        ApathyDrive.PubSub.subscribe(self, "monster_templates:#{mobile.monster_template_id}:monsters")
      end

      if mobile.unity do
        ApathyDrive.PubSub.subscribe(self, "#{mobile.unity}-unity:mobiles")
        mobile = TimerManager.send_every(mobile, {:unify,  60_000, :unify})
      end

      send(self, :notify_presence)

      send(self, :save)

    {:ok, mobile}
  end
  def init(%Mobile{spirit: spirit_id, socket: socket} = mobile) do
    Process.monitor(socket)
    Process.register(self, :"spirit_#{spirit_id}")

    spirit = Repo.get(Spirit, spirit_id)
             |> Repo.preload(:class)

    ApathyDrive.PubSub.subscribe(self, "spirits:online")
    ApathyDrive.PubSub.subscribe(self, "spirits:#{spirit.id}")
    ApathyDrive.PubSub.subscribe(self, "chat:gossip")
    ApathyDrive.PubSub.subscribe(self, "chat:#{String.downcase(spirit.class.name)}")
    ApathyDrive.PubSub.subscribe(self, "rooms:#{spirit.room_id}:spirits")

    ApathyDrive.PubSub.subscribe(socket, "spirits:#{spirit.id}:socket")

    ApathyDrive.PubSub.subscribe(self, "#{spirit.unity}-unity:mobiles")

    mobile =
      mobile
      |> Map.put(:spirit, spirit)
      |> Map.put(:pid, self)
      |> Map.put(:room_id, spirit.room_id)
      |> Map.put(:alignment, spirit.class.alignment)
      |> Map.put(:name, spirit.name)
      |> Map.put(:experience, spirit.experience)
      |> Map.put(:level, spirit.level)
      |> set_abilities
      |> set_max_mana
      |> set_mana
      |> set_max_hp
      |> set_hp
      |> TimerManager.send_every({:monster_regen,    1_000, :regen})
      |> TimerManager.send_every({:periodic_effects, 3_000, :apply_periodic_effects})
      |> TimerManager.send_every({:monster_ai,       5_000, :think})
      |> TimerManager.send_every({:monster_present,  4_000, :notify_presence})
      |> TimerManager.send_every({:unify,  60_000, :unify})

    ApathyDrive.PubSub.subscribe(self, "rooms:#{mobile.room_id}:mobiles")
    ApathyDrive.PubSub.subscribe(self, "rooms:#{mobile.room_id}:mobiles:#{mobile.alignment}")

    update_prompt(mobile)

    ApathyDrive.Endpoint.broadcast_from! self, "spirits:online", "scroll", %{:html => "<p>#{spirit.name} just entered the Realm.</p>"}

    send(self, :notify_presence)

    send(self, :save)

    {:ok, mobile}
  end

  def load(id) do
    case Supervisor.start_child(ApathyDrive.Supervisor, {"mobile##{id}", {GenServer, :start_link, [Mobile, id, [name: {:global, "mobile##{id}"}]]}, :transient, 5000, :worker, [Mobile]}) do
      {:error, {:already_started, pid}} ->
        pid
      {:ok, pid} ->
        pid
    end
  end

  def set_attack_target(%Mobile{attack_target: attack_target} = mobile, target) when attack_target == target do
    mobile
  end
  def set_attack_target(%Mobile{} = mobile, target) do
    Map.put(mobile, :attack_target, target)
  end

  def initiate_combat(%Mobile{timers: %{auto_attack_timer: _}} = mobile) do
    mobile
  end
  def initiate_combat(%Mobile{} = mobile) do
    send(mobile.pid, :execute_auto_attack)
    mobile
  end

  def attack(mobile, target) do
    GenServer.cast(mobile, {:attack, target})
  end

  def possess(mobile, query) do
    GenServer.cast(mobile, {:possess, query})
  end

  def become_possessed(mobile, spirit_id, class, socket, possessor) do
    GenServer.cast(mobile, {:become_possessed, spirit_id, class, socket, possessor})
  end

  def possession_successful(mobile) do
    GenServer.cast(mobile, :possession_successful)
  end

  def turn(mobile, unity, essence) when is_pid(mobile) do
    GenServer.cast(mobile, {:turn, unity, essence})
  end

  def answer(mobile, asker, question) do
    GenServer.cast(mobile, {:answer, asker, question})
  end

  def unpossess(mobile) when is_pid(mobile) do
    GenServer.call(mobile, :unpossess)
  end

  def set_abilities(%Mobile{monster_template_id: nil, spirit: spirit} = mobile) do
    abilities =
     ApathyDrive.ClassAbility.for_spirit(spirit)
     |> add_abilities_from_equipment(spirit.equipment)

    mobile
    |> Map.put(:abilities, abilities)
    |> set_passive_effects
    |> adjust_mana_costs
  end
  def set_abilities(%Mobile{monster_template_id: mt_id, spirit: nil} = mobile) do
    abilities = MonsterTemplate.abilities(mt_id)

    mobile
    |> Map.put(:abilities, abilities)
    |> set_passive_effects
    |> adjust_mana_costs
  end
  def set_abilities(%Mobile{monster_template_id: mt_id, spirit: spirit} = mobile) do
    spirit_abilities =
     ApathyDrive.ClassAbility.for_spirit(spirit)
     |> add_abilities_from_equipment(spirit.equipment)
     |> Enum.filter(fn(ability) ->
          !!Map.get(ability, "passive")
        end)

    monster_abilities = MonsterTemplate.abilities(mt_id)

    mobile
    |> Map.put(:abilities, spirit_abilities ++ monster_abilities)
    |> set_passive_effects
    |> adjust_mana_costs
  end


  def add_abilities_from_equipment(abilities, equipment) do
    abilities ++ Enum.flat_map(equipment, &(&1["abilities"]))
  end

  def set_passive_effects(%Mobile{abilities: []} = mobile) do
    remove_passive_effects(mobile, passive_effects(mobile))
  end
  def set_passive_effects(%Mobile{abilities: abilities} = mobile) do
    original_passives = passive_effects(mobile)

    new_passives =
      abilities
      |> Enum.filter(&(Map.has_key?(&1, "passive_effects")))
      |> Enum.map(&(%{"name" => "passive_#{&1["name"]}", "passive_effects" => &1["passive_effects"]}))


    new_passive_names = Enum.map(new_passives, &(Map.get(&1, "name")))
    passives_to_remove = original_passives -- new_passive_names
    passive_names_to_add = new_passive_names -- original_passives

    mobile = remove_passive_effects(mobile, passives_to_remove)

    passives_to_add =
      Enum.reduce(passive_names_to_add, [], fn(passive_name, to_add) ->
        [Enum.find(new_passives, &(&1["name"] == passive_name)) | to_add]
      end)

    mobile = passives_to_add
    |> Enum.reduce(mobile, fn(%{"name" => name, "passive_effects" => effect}, new_mobile) ->
         Systems.Effect.add_effect(new_mobile, name, effect)
       end)

    mobile
  end

  def remove_passive_effects(%Mobile{} = mobile, effect_keys_to_remove) do
    Enum.reduce(effect_keys_to_remove, mobile, fn(effect_key, new_mobile) ->
      Systems.Effect.remove(new_mobile, effect_key, show_expiration_message: true)
    end)
  end

  def passive_effects(%Mobile{effects: effects}) do
    effects
    |> Map.keys
    |> Enum.filter(&(String.starts_with?(to_string(&1), "passive")))
  end

  def adjust_mana_costs(%Mobile{} = mobile) do
    abilities =
      mobile.abilities
      |> Enum.map(&(adjust_mana_cost(mobile, &1)))

    Map.put(mobile, :abilities, abilities)
  end
  def adjust_mana_cost(%Mobile{} = mobile, %{"mana_cost" => base} = ability) do
    Map.put(ability, "mana_cost",  trunc(base + base * ((level(mobile) * 0.1) * ((level(mobile) * 0.1)))))
  end
  def adjust_mana_cost(%Mobile{}, %{} = ability), do: ability

  def set_mana(%Mobile{mana: nil, max_mana: max_mana} = mobile) do
    Map.put(mobile, :mana, max_mana)
  end
  def set_mana(%Mobile{mana: mana, max_mana: max_mana} = mobile) do
    Map.put(mobile, :mana, min(mana, max_mana))
  end

  def set_max_mana(%Mobile{} = mobile) do
    attr = div((will(mobile) * 2) + agility(mobile), 3)
    Map.put(mobile, :max_mana, trunc(attr * (0.18 + (0.018 * level(mobile)))))
  end

  def set_hp(%Mobile{hp: nil, max_hp: max_hp} = mobile) do
    Map.put(mobile, :hp, max_hp)
  end
  def set_hp(%Mobile{hp: hp, max_hp: max_hp} = mobile) do
    Map.put(mobile, :hp, min(hp, max_hp))
  end

  def set_max_hp(%Mobile{} = mobile) do
    attr = div((strength(mobile) * 2) + agility(mobile), 3)
    Map.put(mobile, :max_hp, trunc(attr * (0.6 + (0.06 * level(mobile)))))
  end

  def level(%Mobile{spirit: nil, level: level}),     do: level
  def level(%Mobile{spirit: %Spirit{level: level}}), do: level

  def weapon(%Mobile{spirit: nil}), do: nil
  def weapon(%Mobile{spirit: %Spirit{equipment: equipment}}) do
    equipment
    |> Enum.find(fn(%{"worn_on" => worn_on}) ->
         worn_on in ["Weapon Hand", "Two Handed"]
       end)
  end

  def physical_damage(%Mobile{} = mobile) do
    str = strength(mobile)
    agi = agility(mobile)
    wil = will(mobile)

    physical_damage(str, agi, wil)
  end

  def physical_damage(str, agi, _wil) do
    ((str * 2) + agi) / 20
  end

  def magical_damage(%Mobile{} = mobile) do
    str = strength(mobile)
    agi = agility(mobile)
    wil = will(mobile)

    magical_damage(str, agi, wil)
  end

  def magical_damage(_str, agi, wil) do
    ((wil * 2) + agi) / 20
  end

  def strength(%Mobile{} = mobile) do
    attribute(mobile, :strength)
  end

  def agility(%Mobile{} = mobile) do
    attribute(mobile, :agility)
  end

  def will(%Mobile{} = mobile) do
    attribute(mobile, :will)
  end

  def attribute(%Mobile{spirit: nil, strength: nil, agility: nil, will: nil, level: level}, _attribute) do
    50 + (10 * level)
  end

  def attribute(%Mobile{spirit: nil, strength: str}, :strength), do: str
  def attribute(%Mobile{spirit: nil, agility:  agi}, :agility),  do: agi
  def attribute(%Mobile{spirit: nil, will:    will}, :will),     do: will

  def attribute(%Mobile{spirit: spirit, monster_template_id: nil} = mobile, attribute) do
    ((level(mobile) - 1) * Map.get(spirit.class, :"#{attribute}_per_level")) +
    Map.get(spirit.class, :"#{attribute}") +
    attribute_from_equipment(mobile, attribute)
  end

  def attribute(%Mobile{} = mobile, attribute) do
    spirit_attribute =
      mobile
      |> Map.put(:monster_template_id, nil)
      |> attribute(attribute)

    mobile_attribute =
      mobile
      |> Map.put(:spirit, nil)
      |> attribute(attribute)

    div((spirit_attribute + mobile_attribute), 2)
  end

  def attribute_from_equipment(%Mobile{spirit: nil}, _), do: 0
  def attribute_from_equipment(%Mobile{spirit: %Spirit{equipment: equipment}}, attribute) do
    Enum.reduce(equipment, 0, &(&2 + apply(ApathyDrive.Item, attribute, [&1])))
  end

  def hp_regen_per_second(%Mobile{max_hp: max_hp} = mobile) do
    modifier = 1 + effect_bonus(mobile, "hp_regen") / 100

    normal_regen = max_hp * 0.01 * modifier

    poison = effect_bonus(mobile, "poison") / 10

    normal_regen - poison
  end

  def mana_regen_per_second(%Mobile{max_mana: max_mana} = mobile) do
    modifier = 1 + effect_bonus(mobile, "mana_regen") / 100

    max_mana * 0.01 * modifier
  end

  def local_hated_targets(%Mobile{hate: hate} = mobile) do
    mobile
    |> Room.mobiles
    |> Enum.reduce(%{}, fn(potential_target, targets) ->
         threat = Map.get(hate, potential_target, 0)
         if threat > 0 do
           Map.put(targets, threat, potential_target)
         else
           targets
         end
       end)
  end

  def global_hated_targets(%Mobile{hate: hate}) do
    hate
    |> Map.keys
    |> Enum.reduce(%{}, fn(potential_target, targets) ->
         threat = Map.get(hate, potential_target, 0)
         if threat > 0 do
           Map.put(targets, threat, potential_target)
         else
           targets
         end
       end)
  end

  def aggro_target(%Mobile{} = mobile) do
    targets = local_hated_targets(mobile)

    top_threat = targets
                 |> Map.keys
                 |> top_threat

    Map.get(targets, top_threat)
  end

  def most_hated_target(%Mobile{} = mobile) do
    targets = global_hated_targets(mobile)

    top_threat = targets
                 |> Map.keys
                 |> top_threat

    Map.get(targets, top_threat)
  end

  def top_threat([]),      do: nil
  def top_threat(targets), do: Enum.max(targets)

  def send_execute_auto_attack do
    send(self, :execute_auto_attack)
  end

  def look_name(%Mobile{} = mobile) do
    "<span class='#{alignment_color(mobile)}'>#{mobile.name}</span>"
  end

  def notify_presence(%Mobile{room_id: room_id} = mobile) do
    data = %{
      intruder: self,
      alignment: mobile.alignment,
      unity: mobile.unity || (mobile.spirit && mobile.spirit.unity),
      spawned_at: mobile.spawned_at,
      name: mobile.name,
      look_name: look_name(mobile)
    }

    room_id
    |> Room.find
    |> Room.notify_presence(data)
  end

  def save(%Mobile{monster_template_id: nil, spirit: spirit} = mobile) do
    Map.put(mobile, :spirit, Repo.save!(spirit))
  end

  def save(%Mobile{monster_template_id: _, spirit: nil} = mobile) do
    Repo.save!(mobile)
  end

  def save(%Mobile{monster_template_id: _, spirit: spirit} = mobile) do
    mobile
    |> Map.put(:spirit, Repo.save!(spirit))
    |> Repo.save!
  end

  defp unify(mobile, unity, essence) do
    essence_to_distribute = div(essence, 100)

    Room.add_essence_from_mobile({:global, :"room_#{mobile.room_id}"}, self(), unity, essence_to_distribute)
    ApathyDrive.Unity.contribute(self(), unity, essence_to_distribute)

    add_experience(mobile, -(essence_to_distribute * 2))
  end

  defp jitter(time) do
    time
    |> :rand.uniform
    |> Kernel.+(time)
  end

  defp should_move?(%Mobile{spirit: nil} = mobile) do
    cond do
      # at least 80% health and no enemies present, go find something to kill
      ((mobile.hp / mobile.max_hp) >= 0.8) and !Enum.any?(local_hated_targets(mobile)) ->
        true
      # 30% or less health and enemies present, run away!
      ((mobile.hp / mobile.max_hp) <= 0.3) and Enum.any?(local_hated_targets(mobile)) ->
        true
      true ->
        false
    end
  end

  defp execute_auto_attack(%Mobile{} = mobile, target) do
    attacks =
      mobile.abilities
      |> Enum.filter(&(&1["kind"] == "auto_attack"))

    attack = if Enum.any?(attacks), do: Enum.random(attacks), else: nil

    if attack do
      attack =
        attack
        |> Map.put("ignores_global_cooldown", true)
        |> Map.put("kind", "attack")

      send(self, {:execute_ability, attack, [target]})
      attack["attack_interval"] # return interval to change default auto_attack delay
    end
  end

  defp move_after(%Mobile{movement_frequency: frequency} = mobile) do
    TimerManager.send_after(mobile, {:monster_movement, jitter(:timer.seconds(frequency)), :auto_move})
  end

  defp worn_on_max(%{"worn_on" => "Finger"}), do: 2
  defp worn_on_max(%{"worn_on" => "Wrist"}),  do: 2
  defp worn_on_max(%{"worn_on" => _}),        do: 1

  defp conflicting_worn_on("Weapon Hand"),     do: ["Two Handed"]
  defp conflicting_worn_on("Off-Hand"),   do: ["Two Handed"]
  defp conflicting_worn_on("Two Handed"), do: ["Weapon Hand", "Off-Hand"]
  defp conflicting_worn_on(_), do: []

  defp list_forms(mobile, forms, limb) do
    alias ApathyDrive.Item

    Mobile.send_scroll(mobile, "<p>\n<span class='white'>You know how to construct the following items:</span></p>")

    forms
    |> Enum.reduce(%{}, fn(item, items) ->
         items
         |> Map.put_new(item.worn_on, [])
         |> update_in([item.worn_on], &([item | &1]))
       end)
    |> Enum.each(fn({slot, items}) ->
         if String.downcase(slot) == String.downcase(limb) or limb == "" do
           Mobile.send_scroll(mobile, "<p><span class='dark-yellow'>#{slot}</span></p>")
           Mobile.send_scroll(mobile, "<p><span class='dark-magenta'>Essence Cost | STR | AGI | WIL | Item Name</span></p>")
           Enum.each(items, fn(item) ->
             exp =
              (ApathyDrive.Item.experience(Item.strength(item) + Item.agility(item) + Item.will(item)) * 10)
              |> to_string
              |> String.ljust(12)

             Mobile.send_scroll(mobile, "<p><span class='dark-cyan'>#{exp} | #{String.rjust(to_string(Item.strength(item)), 3)} | #{String.rjust(to_string(Item.agility(item)), 3)} | #{String.rjust(to_string(Item.will(item)), 3)} | #{item.name}</span></p>")
           end)
           Mobile.send_scroll(mobile, "<p>\n</p>")
         end
       end)
  end

  def handle_call(:unpossess, _from, %Mobile{monster_template_id: nil} = mobile) do
    {:reply, {:error, "You aren't possessing anything."}, mobile}
  end
  def handle_call(:unpossess, _from, %Mobile{socket: _socket, spirit: spirit} = mobile) do
    mobile =
      mobile
      |> Map.put(:spirit, nil)
      |> Map.put(:socket, nil)
      |> set_abilities
      |> set_max_mana
      |> set_mana
      |> set_max_hp
      |> set_hp

      Process.unregister(:"spirit_#{spirit.id}")

      ApathyDrive.PubSub.unsubscribe(self, "spirits:online")
      ApathyDrive.PubSub.unsubscribe(self, "spirits:#{spirit.id}")
      ApathyDrive.PubSub.unsubscribe(self, "chat:gossip")
      ApathyDrive.PubSub.unsubscribe(self, "chat:#{String.downcase(spirit.class.name)}")

    {:reply, {:ok, spirit: spirit, mobile_name: mobile.name}, mobile}
  end

  def handle_call({:unequip_item, item}, _from, %Mobile{spirit: %Spirit{inventory: inventory, equipment: equipment}} = mobile) do
    item = equipment
           |> Enum.map(&(%{name: &1["name"], keywords: String.split(&1["name"]), item: &1}))
           |> Match.one(:keyword_starts_with, item)

    case item do
      nil ->
        {:reply, :not_found, mobile}
      %{item: item_to_remove} ->
        equipment =
          equipment
          |> List.delete(item_to_remove)

        inventory =
          inventory
          |> List.insert_at(-1, item_to_remove)

          mobile = put_in(mobile.spirit.inventory, inventory)
          mobile = put_in(mobile.spirit.equipment, equipment)
                   |> set_abilities
                   |> set_max_mana
                   |> set_mana
                   |> set_max_hp
                   |> set_hp

        {:reply, {:ok, %{unequipped: item_to_remove}}, save(mobile)}
    end
  end

  def handle_cast({:attack, target}, mobile) when is_pid(target) do
    {:noreply, Commands.Attack.attack(mobile, target)}
  end

  def handle_cast({:attack, target}, mobile) do
    Commands.Attack.execute(mobile, target)
    {:noreply, mobile}
  end

  def handle_cast({:say, message}, mobile) do
    Commands.Say.execute(mobile, message)
    {:noreply, mobile}
  end

  def handle_cast({:gossip, message}, mobile) do
    Commands.Gossip.execute(mobile, message)
    {:noreply, mobile}
  end

  def handle_cast({:ask, target, question}, mobile) do
    Commands.Ask.execute(mobile, target, question)
    {:noreply, mobile}
  end

  def handle_cast(:possession_successful, mobile) do
    {:stop, :normal, Systems.Effect.remove_all(mobile)}
  end

  def handle_cast({:possess, query}, mobile) do
    Commands.Possess.execute(mobile, query)
    {:noreply, mobile}
  end

  def handle_cast({:become_possessed, spirit_id, class, socket, possessor}, mobile) do
    {:noreply, Commands.Possess.become_possessed(mobile, spirit_id, class, socket, possessor)}
  end

  def handle_cast({:look_at_mobile, looker}, mobile) do
    Commands.Look.look_at_mobile(mobile, looker)
    {:noreply, mobile}
  end

  def handle_cast({:look_at_item, item}, mobile) do
    Commands.Look.look_at_item(mobile, item)
    {:noreply, mobile}
  end

  def handle_cast({:equip_item, item}, mobile) do
    {:noreply, Commands.Wear.execute(mobile, item)}
  end

  def handle_cast({:get_item, %{} = item}, mobile) do
    {:noreply, Commands.Get.execute(mobile, item)}
  end

  def handle_cast({:get_item, item}, mobile) do
    Commands.Get.execute(mobile, item)
    {:noreply, mobile}
  end

  def handle_cast({:drop_item, item_name}, %Mobile{spirit: %Spirit{inventory: inventory}} = mobile) do
    item = inventory
           |> Enum.map(&(%{name: &1["name"], keywords: String.split(&1["name"]), item: &1}))
           |> Match.one(:name_contains, item_name)

    case item do
      nil ->
        Mobile.send_scroll(mobile, "<p>You don't have \"#{item_name}\" to drop!</p>")
        {:noreply, mobile}
      %{item: item} ->
        mobile =
          put_in(mobile.spirit.inventory, List.delete(inventory, item))

          Mobile.send_scroll(mobile, "<p>You drop #{item["name"]}.</p>")

          mobile.room_id
          |> Room.find
          |> Room.add_item(item)

        {:noreply, save(mobile)}
    end
  end

  def handle_cast(:show_score, mobile) do
    Commands.Score.execute(mobile)
    {:noreply, mobile}
  end

  def handle_cast({:absorb, item}, mobile) do
    {:noreply, Commands.Absorb.execute(mobile, item)}
  end

  def handle_cast(:display_inventory, mobile) do
    Commands.Inventory.execute(mobile)
    {:noreply, mobile}
  end

  def handle_cast({:bash, args}, mobile) do
    Commands.Bash.execute(mobile, args)
    {:noreply, mobile}
  end

  def handle_cast({:execute_room_command, scripts}, mobile) do
    unless confused(mobile) do
      scripts = Enum.map(scripts, &ApathyDrive.Script.find/1)
      Mobile.execute_script(mobile, scripts)
    end

    {:noreply, mobile}
  end

  def handle_cast({:trigger_remote_action, remote_action_exit}, mobile) do
    unless confused(mobile) do
      remote_action_exit["destination"]
      |> Room.find
      |> Room.trigger_remote_action(remote_action_exit, mobile.room_id)

      mobile.room_id
      |> Room.send_scroll(%{
        self => "<p>#{remote_action_exit["message"]}</p>",
        :other => "<p>#{interpolate(remote_action_exit["room_message"], %{"name" => look_name(mobile)})}</span></p>"
      })
    end

    {:noreply, mobile}
  end

  def handle_cast({:move, room, room_exit, last_room}, mobile) do
    mobile = Commands.Move.execute(mobile, room, room_exit, last_room)
    {:noreply, mobile}
  end

  def handle_cast({:execute_command, command, arguments}, mobile) do
    ApathyDrive.Command.execute(mobile, command, arguments)
    {:noreply, mobile}
  end

  def handle_cast({:look, args}, mobile) do
    Commands.Look.execute(mobile, args)
    {:noreply, mobile}
  end

  def handle_cast({:display_enter_message, room, _direction}, mobile) do
    Room.display_enter_message(room, %{name: look_name(mobile), mobile: mobile, message: mobile.enter_message, from: mobile.room_id})
    {:noreply, mobile}
  end

  def handle_cast({:auto_move, %{new_exit: room_exit, last_room: _last_room}}, mobile) do
    mobile.room_id
    |> Room.find
    |> Room.execute_command(self, room_exit["direction"], [])
    #Commands.Move.execute(Room.find(mobile.room_id), self, room_exit, last_room)
    {:noreply, mobile}
  end
  def handle_cast({:auto_move, _}, mobile) do
    {:noreply, mobile}
  end

  def handle_cast({:answer, asker, question}, mobile) do
    Commands.Ask.answer(mobile, asker, question)
    {:noreply, mobile}
  end

  def handle_cast({:greet, %{name: _, pid: _} = greeter}, mobile) do
    Commands.Greet.greet(mobile, greeter)
    {:noreply, mobile}
  end

  def handle_cast({:greet, target}, mobile) do
    Commands.Greet.greet(mobile, target)
    {:noreply, mobile}
  end

  def handle_cast(:display_abilities, mobile) do
    Commands.Abilities.display_abilities(mobile)
    {:noreply, mobile}
  end

  def handle_cast({:turn, unity, essence}, %Mobile{} = mobile) do
    mobile =
      mobile
      |> Map.put(:attack_target, nil)
      |> Map.put(:hate, %{})
      |> Map.put(:unity, unity)
      |> Map.put(:alignment, (if unity == "angel", do: "good", else: "evil"))
      |> Map.put(:experience, essence)
      |> Repo.save!

    ApathyDrive.PubSub.subscribe(self, "#{unity}-unity:mobiles")

    {:noreply, mobile}
  end

  def handle_cast({:execute_script, script}, mobile) do
    {:noreply, ApathyDrive.Script.execute(script, mobile)}
  end

  def handle_cast(:display_cooldowns, mobile) do
    mobile.effects
    |> Map.values
    |> Enum.filter(fn(effect) ->
         Map.has_key?(effect, "cooldown")
       end)
    |> Enum.each(fn
           %{"cooldown" => name} = effect when is_binary(name) ->
             remaining =
               mobile
               |> ApathyDrive.TimerManager.time_remaining(effect["timers"] |> List.first)
               |> div(1000)

             Mobile.send_scroll(mobile, "<p><span class='dark-cyan'>#{name |> String.ljust(15)} #{remaining} seconds</span></p>")
          _effect ->
            :noop
       end)
    {:noreply, mobile}
  end

  def handle_cast({:use_ability, command, args}, mobile) do

    ability = mobile.abilities
              |> Enum.find(fn(ability) ->
                   ability["command"] == String.downcase(command)
                 end)

    if ability do
      mobile = Ability.execute(mobile, ability, Enum.join(args, " "))
      {:noreply, mobile}
    else
      Mobile.send_scroll(mobile, "<p>What?</p>")
      {:noreply, mobile}
    end
  end

  def handle_cast({:add_experience, exp}, %Mobile{} = mobile) do
    mobile = add_experience(mobile, exp)

    {:noreply, mobile}
  end

  def handle_cast({:construct_item, item_name}, mobile) do
    alias ApathyDrive.Item

    match =
      mobile
      |> forms()
      |> Enum.map(&(%{name: &1.name, keywords: String.split(&1.name), item: &1}))
      |> Match.one(:name_contains, item_name)

    if match do
      item = match.item

      cost = Item.experience(Item.strength(item) + Item.agility(item) + Item.will(item)) * 10

      cond do
        remaining_encumbrance(mobile) < item.weight ->
          Mobile.send_scroll(mobile, "<p>A #{item.name} would be too heavy for you to hold.</p>")
          {:noreply, mobile}
        cost > mobile.spirit.experience ->
          Mobile.send_scroll(mobile, "<p>You don't have enough essence to construct #{item.name}.</p>")
          {:noreply, mobile}
        true ->
          constructed_item = Item.generate_item(%{item_id: item.id, level: level(mobile)})

          mobile =
            put_in(mobile.spirit.experience, mobile.spirit.experience - cost)

          mobile =
            put_in(mobile.spirit.inventory, [constructed_item | mobile.spirit.inventory])

            Repo.save!(mobile.spirit)

          Mobile.send_scroll(mobile, "<p>You construct a #{item.name} using #{cost} of your essence.</p>")

          {:noreply, mobile}
      end
    else
      Mobile.send_scroll(mobile, "<p>You don't know how to construct a #{item_name}.</p>")
      {:noreply, mobile}
    end
  end

  def handle_cast({:list_forms, limb}, %Mobile{} = mobile) do
    list_forms(mobile, forms(mobile), limb)

    {:noreply, mobile}
  end

  def handle_cast({:add_form, %{"id" => item_id, "name" => name}}, %Mobile{spirit: spirit} = mobile) do
    alias ApathyDrive.SpiritItemRecipe

    form =
      %SpiritItemRecipe{}
      |> SpiritItemRecipe.changeset(%{item_id: item_id, spirit_id: spirit.id})

    case Repo.insert(form) do
      {:ok, _recipe} ->
        Mobile.send_scroll(mobile, "<p>You gain knowledge of #{name}'s <span class='green'>form</span>, allowing you to <span class='green'>construct</span> it from raw essence.</p>")
      {:error, _} ->
        :noop
    end

    {:noreply, mobile}
  end

  def handle_cast(:display_experience, %Mobile{spirit: nil} = mobile) do
    {:noreply, mobile}
  end
  def handle_cast(:display_experience, %Mobile{spirit: spirit} = mobile) do
    Mobile.send_scroll(mobile, Commands.Experience.message(spirit))

    {:noreply, mobile}
  end

  def handle_cast({:class_chat, _message}, %Mobile{spirit: nil} = mobile) do
    {:noreply, mobile}
  end
  def handle_cast({:class_chat, message}, %Mobile{spirit: spirit} = mobile) do
    class_name = String.downcase(spirit.class.name)

    ApathyDrive.PubSub.broadcast!("chat:#{class_name}", {String.to_atom(class_name), Mobile.aligned_spirit_name(mobile), message})
    {:noreply, mobile}
  end

  def handle_info({:scroll, %{} = data}, mobile) do
    if self in Map.keys(data) do
      send_scroll(mobile, data[self])
    else
      send_scroll(mobile, data[:other])
    end
    {:noreply, mobile}
  end

  def handle_info({:scroll, html}, mobile) do
    send_scroll(mobile, html)
    {:noreply, mobile}
  end

  def handle_info(:save, mobile) do
    Process.send_after(self, :save, jitter(:timer.minutes(5)))
    {:noreply, save(mobile)}
  end

  def handle_info({:who_request, who}, mobile) do
    Mobile.send_scroll(who, "<p>#{Mobile.look_name(mobile)}</p>")
    {:noreply, mobile}
  end

  def handle_info({:timer_cast_ability, %{ability: ability, timer: time, target: target}}, mobile) do
    Mobile.send_scroll(mobile, "<p><span class='dark-yellow'>You cast your spell.</span></p>")

    ability = case ability do
      %{"global_cooldown" => nil} ->
        ability
      %{"global_cooldown" => cooldown} ->
        if cooldown > time do
          Map.put(ability, "global_cooldown", cooldown - time)
        else
          ability
          |> Map.delete("global_cooldown")
          |> Map.put("ignores_global_cooldown", true)
        end
      _ ->
        ability
    end

    send(self, {:execute_ability, Map.delete(ability, "cast_time"), target})

    {:noreply, mobile}
  end

  def handle_info({:execute_ability, ability}, monster) do
    {:noreply, Ability.execute(monster, ability, [self])}
  end

  def handle_info({:execute_ability, ability, arg_string}, mobile) do
    mobile = Ability.execute(mobile, ability, arg_string)
    {:noreply, mobile}
  end

  def handle_info({:send_scroll, message}, mobile) do
    send_scroll(mobile, message)

    {:noreply, mobile}
  end

  def handle_info(:auto_move, %Mobile{movement: "stationary"} = mobile) do
    {:noreply, move_after(mobile)}
  end
  def handle_info(:auto_move, %Mobile{spirit: nil} = mobile) do
    if should_move?(mobile) && (room = Room.find(mobile.room_id)) do
      Room.auto_move(room, self, mobile.last_room)
    end
    {:noreply, move_after(mobile)}
  end
  def handle_info(:auto_move, mobile) do
    {:noreply, move_after(mobile)}
  end

  def handle_info({:execute_script, script}, mobile) do
    {:noreply, ApathyDrive.Script.execute(script, Map.put(mobile, :delayed, false))}
  end

  def handle_info(%Phoenix.Socket.Broadcast{}, %Mobile{socket: nil} = mobile) do
    {:noreply, mobile}
  end
  def handle_info(%Phoenix.Socket.Broadcast{} = message, %Mobile{socket: socket} = mobile) do
    send(socket, message)

    {:noreply, mobile}
  end

  def handle_info(:think, mobile) do
    {:noreply, ApathyDrive.AI.think(mobile)}
  end

  def handle_info({:apply_ability, %{} = ability, %Mobile{} = ability_user}, mobile) do
    if Ability.affects_target?(mobile, ability) do
      mobile = mobile
               |> Ability.apply_ability(ability, ability_user)

      Mobile.update_prompt(mobile)

      if mobile.hp < 1 do
        {:noreply, Systems.Death.kill(mobile)}
      else
        {:noreply, mobile}
      end
    else
      message = "#{mobile.name} is not affected by that ability." |> capitalize_first
      Mobile.send_scroll(ability_user, "<p><span class='dark-cyan'>#{message}</span></p>")
      {:noreply, mobile}
    end
  end

  def handle_info({:timeout, _ref, {name, time, [module, function, args]}}, %Mobile{timers: timers} = mobile) do
    jitter = trunc(time / 2) + :random.uniform(time)

    new_ref = :erlang.start_timer(jitter, self, {name, time, [module, function, args]})

    timers = Map.put(timers, name, new_ref)

    apply module, function, args

    {:noreply, Map.put(mobile, :timers, timers)}
  end

  def handle_info({:timeout, _ref, {name, [module, function, args]}}, %Mobile{timers: timers} = mobile) do
    apply module, function, args

    timers = Map.delete(timers, name)

    {:noreply, Map.put(mobile, :timers, timers)}
  end

  def handle_info({:remove_effect, key}, mobile) do
    mobile = Systems.Effect.remove(mobile, key, fire_after_cast: true, show_expiration_message: true)
    {:noreply, mobile}
  end

  def handle_info(:regen, %Mobile{hp: hp, max_hp: max_hp, mana: mana, max_mana: max_mana} = mobile) do
    mobile = mobile
             |> Map.put(:hp,   min(  hp + hp_regen_per_second(mobile), max_hp))
             |> Map.put(:mana, min(mana + mana_regen_per_second(mobile), max_mana))

    update_prompt(mobile)

    {:noreply, mobile}
  end

  def handle_info({:mobile_died, mobile: %Mobile{}, reward: _exp}, %Mobile{unity: nil, spirit: nil} = mobile) do
    {:noreply, mobile}
  end
  def handle_info({:mobile_died, mobile: %Mobile{} = _deceased, reward: exp}, %Mobile{spirit: nil} = mobile) do
    mobile = add_experience(mobile, exp)

    {:noreply, mobile}
  end
  def handle_info({:mobile_died, mobile: %Mobile{} = deceased, reward: exp}, %Mobile{spirit: %Spirit{} = spirit} = mobile) do
    message = deceased.death_message
              |> interpolate(%{"name" => deceased.name})
              |> capitalize_first

    send_scroll(mobile, "<p>#{message}</p>")

    send_scroll(mobile, "<p>You gain #{exp} essence.</p>")

    new_spirit =
      spirit
      |> Spirit.add_experience(exp)

    if new_spirit.level > spirit.level do
      mobile = mobile
               |> Map.put(:spirit, new_spirit)
               |> set_abilities

      send_scroll(mobile, "<p>You've advanced to level #{new_spirit.level}!</p>")

      {:noreply, mobile}
    else
      mobile = mobile
                |> Map.put(:spirit, new_spirit)

      {:noreply, mobile}
    end
  end

  def handle_info({:gossip, name, message}, mobile) do
    send_scroll(mobile, "<p>[<span class='dark-magenta'>gossip</span> : #{name}] #{message}</p>")

    {:noreply, mobile}
  end

  def handle_info({:angel, name, message}, mobile) do
    send_scroll(mobile, "<p>[<span class='white'>angel</span> : #{name}] #{message}</p>")
    {:noreply, mobile}
  end

  def handle_info({:elemental, name, message}, mobile) do
    send_scroll(mobile, "<p>[<span class='dark-cyan'>elemental</span> : #{name}] #{message}</p>")
    {:noreply, mobile}
  end

  def handle_info({:demon, name, message}, mobile) do
    send_scroll(mobile, "<p>[<span class='magenta'>demon</span> : #{name}] #{message}</p>")
    {:noreply, mobile}
  end

  def handle_info(:apply_periodic_effects, mobile) do

    # periodic damage
    mobile.effects
    |> Map.values
    |> Enum.filter(&(Map.has_key?(&1, "damage")))
    |> Enum.each(fn(%{"damage" => damage, "effect_message" => message}) ->
         ability = %{"kind" => "attack",
                     "ignores_global_cooldown" => true,
                     "flags" => [],
                     "instant_effects" => %{"damage" => damage},
                     "cast_message"    => %{"user" => message}}

         send(self, {:apply_ability, ability, mobile})
       end)

    # # periodic heal
    # monster.effects
    # |> Map.values
    # |> Enum.filter(&(Map.has_key?(&1, "heal")))
    # |> Enum.each(fn(%{"heal" => heal}) ->
    #     ability = %Ability{kind: "heal", global_cooldown: nil, flags: [], properties: %{"instant_effects" => %{"heal" => heal}}}
    #
    #     send(self, {:apply_ability, ability, monster})
    #   end)
    #
    # # periodic heal_mana
    # monster.effects
    # |> Map.values
    # |> Enum.filter(&(Map.has_key?(&1, "heal_mana")))
    # |> Enum.each(fn(%{"heal_mana" => heal}) ->
    #     ability = %Ability{kind: "heal", global_cooldown: nil, flags: [], properties: %{"instant_effects" => %{"heal_mana" => heal}}}
    #
    #     send(self, {:apply_ability, ability, monster})
    #   end)

    {:noreply, mobile}
  end

  def handle_info(:execute_auto_attack, %Mobile{attack_target: nil} = mobile) do
    {:noreply, mobile}
  end
  def handle_info(:execute_auto_attack, %Mobile{attack_target: target, auto_attack_interval: default_interval} = mobile) do
    if Process.alive?(target) and target in PubSub.subscribers("rooms:#{mobile.room_id}:mobiles") do
      attack_interval = execute_auto_attack(mobile, target)

      interval = attack_interval || seconds(default_interval)

      mobile = TimerManager.call_after(mobile, {:auto_attack_timer, interval, [__MODULE__, :send_execute_auto_attack, []]})

      {:noreply, mobile}
    else
      mobile =
        mobile
        |> Map.put(:attack_target, nil)

      {:noreply, mobile}
    end
  end

  def handle_info(:notify_presence, %Mobile{} = mobile) do
    notify_presence(mobile)

    {:noreply, mobile}
  end

  def handle_info(:unify, %Mobile{spirit: nil, unity: unity, experience: essence} = mobile) when unity in ["demon", "angel"] do
    {:noreply, unify(mobile, unity, essence)}
  end

  def handle_info(:unify, %Mobile{spirit: %Spirit{class: %Class{name: class}}, experience: essence} = mobile) when class in ["Demon", "Angel"] do
    {:noreply, unify(mobile, String.downcase(class), essence)}
  end

  def handle_info({:monster_present, %{} = intruder_data}, %Mobile{spirit: nil} = mobile) do
    mobile = ApathyDrive.Aggression.react(%{mobile: mobile,
                                            alignment: mobile.alignment,
                                            unity: mobile.unity || (mobile.spirit && mobile.spirit.unity),
                                            spawned_at: mobile.spawned_at,
                                            name: mobile.name
                                          },
                                          intruder_data)

    {:noreply, mobile}
  end

  def handle_info({:DOWN, _ref, :process, pid, {:normal, :timeout}}, %Mobile{spirit: _spirit, socket: socket} = mobile) when pid == socket do
    send(self, :disconnected)
    {:noreply, Map.put(mobile, :socket, nil)}
  end

  def handle_info({:DOWN, _ref, :process, pid, _reason}, %Mobile{spirit: _spirit, socket: socket} = mobile) when pid == socket do
    Process.send_after(self, :disconnected, 30_000)
    {:noreply, Map.put(mobile, :socket, nil)}
  end

  def handle_info(:disconnected, %Mobile{monster_template_id: nil, spirit: spirit, socket: nil} = mobile) do
    ApathyDrive.Endpoint.broadcast! "spirits:online", "scroll", %{:html => "<p>#{spirit.name} just left the Realm.</p>"}
    {:stop, :normal, save(mobile)}
  end

  def handle_info(:disconnected, %Mobile{spirit: spirit, socket: nil} = mobile) do
    ApathyDrive.Endpoint.broadcast! "spirits:online", "scroll", %{:html => "<p>#{spirit.name} just left the Realm.</p>"}
    mobile =
      mobile
      |> Map.put(:spirit, nil)

      ApathyDrive.PubSub.unsubscribe(self, "spirits:online")
      ApathyDrive.PubSub.unsubscribe(self, "spirits:#{spirit.id}")
      ApathyDrive.PubSub.unsubscribe(self, "chat:gossip")
      ApathyDrive.PubSub.unsubscribe(self, "chat:#{String.downcase(spirit.class.name)}")

    {:noreply, mobile}
  end

  def handle_info({:set_socket, socket}, %Mobile{socket: nil} = mobile) do
    Process.monitor(socket)
    {:noreply, Map.put(mobile, :socket, socket)}
  end

  # already signed-in in another tab / browser / location whatever
  # redirect old socket to the home page and give control to the new socket
  def handle_info({:set_socket, socket}, %Mobile{socket: old_socket} = mobile) do
    Process.monitor(socket)

    if socket != old_socket, do: send(old_socket, :go_home)

    {:noreply, Map.put(mobile, :socket, socket)}
  end

  def handle_info(:die, mobile) do
    Systems.Death.kill(mobile)

    {:noreply, mobile}
  end

  def handle_info({:execute_room_ability, ability}, mobile) do
    ability = Map.put(ability, "ignores_global_cooldown", true)

    {:noreply, Ability.execute(mobile, ability, [self])}
  end

  def handle_info({:generate_loot, monster_template_id, level}, mobile) do
    ItemDrop.monster_drops(monster_template_id)
    |> Enum.map(fn(%{item_id: item_id, chance: chance}) ->
         ApathyDrive.Item.generate_item(%{chance: chance, item_id: item_id, level: level})
       end)
    |> List.insert_at(0, ApathyDrive.Item.generate_item(%{chance: 50, item_id: :global, level: level}))
    |> Enum.reject(&is_nil/1)
    |> case do
         [] ->
           {:noreply, mobile}
         items ->
           Mobile.send_scroll(mobile, "<p>\n<span class='white'>A wild surge of spirtual essence coalesces into:</span></p>")
           Mobile.send_scroll(mobile, "<p><span class='dark-magenta'>STR | AGI | WIL | Item Name</span></p>")
           Enum.each(items, fn(item) ->

             current =
               mobile
               |> Mobile.score_data

             %{equipped: _, mobile: equipped} =
               mobile
               |> Mobile.equip_item(item)

             equipped = Mobile.score_data(equipped)

             color =
               current
               |> Map.take([:strength, :agility, :will])
               |> Enum.reduce(%{}, fn({key, val}, values) ->
                    diff = equipped[key] - val
                    color = cond do
                      diff > 0 ->
                        "green"
                      diff < 0 ->
                        "dark-red"
                      true ->
                        "dark-cyan"
                    end
                    Map.put(values, key, color)
                  end)


             Mobile.send_scroll(mobile, "<p><span class='#{color[:strength]}'>#{String.rjust(to_string(Item.strength(item)), 3)}</span> <span class='dark-cyan'>|</span> <span class='#{color[:agility]}'>#{String.rjust(to_string(Item.agility(item)), 3)}</span> <span class='dark-cyan'>|</span> <span class='#{color[:will]}'>#{String.rjust(to_string(Item.will(item)), 3)}</span> <span class='dark-cyan'>| #{item["name"]}</span></p>")
           end)

           mobile = put_in(mobile.spirit.inventory, items ++ mobile.spirit.inventory)

           Repo.save!(mobile.spirit)

           {:noreply, mobile}
       end
  end

  def handle_info({:say, %{name: _speaker, unity: speaker_unity}, "stay"}, %Mobile{spirit: nil, unity: unity} = mobile) when unity == speaker_unity do
    ApathyDrive.Endpoint.broadcast_from! self, "rooms:#{mobile.room_id}:mobiles", "scroll", %{html: "<p>#{capitalize_first(mobile.name)} says: <span class='dark-green'>\"Ok.\"</span></p>"}

    mobile =
      mobile
      |> Map.put(:movement, "stationary")
      |> Repo.save!

    {:noreply, mobile}
  end

  def handle_info({:say, %{name: _speaker, unity: speaker_unity}, "hunt"}, %Mobile{spirit: nil, unity: unity} = mobile) when unity == speaker_unity do
    ApathyDrive.Endpoint.broadcast_from! self, "rooms:#{mobile.room_id}:mobiles", "scroll", %{html: "<p>#{capitalize_first(mobile.name)} says: <span class='dark-green'>\"Ok.\"</span></p>"}

    mobile =
      mobile
      |> Map.put(:movement, "solo")
      |> Map.put(:last_room, nil)
      |> Repo.save!

    {:noreply, mobile}
  end

  def handle_info({:say, %{name: speaker, unity: _speaker_unity}, message}, %Mobile{} = mobile) do
    Mobile.send_scroll(mobile, "<p>#{capitalize_first(speaker)} says: <span class='dark-green'>\"#{message}\"</span></p>")
    {:noreply, mobile}
  end

  def handle_info({:mobile_movement, %{mobile: mover, room: room, message: message}}, %Mobile{room_id: room_id} = mobile) when room == room_id and mover != self() do
    send_scroll mobile, message
    {:noreply, mobile}
  end

  def handle_info({:door_bashed, %{basher: pid, type: type}}, mobile) when pid == self() do
    Mobile.send_scroll(mobile, "<p>You bashed the #{type} open.</p>")
    {:noreply, mobile}
  end

  def handle_info({:door_bashed, %{name: name, type: type, description: description}}, mobile) do
    Mobile.send_scroll(mobile, "<p>You see #{name} bash open the #{type} #{description}.</p>")
    {:noreply, mobile}
  end

  def handle_info(_message, %Mobile{} = mobile) do
    {:noreply, mobile}
  end

end
