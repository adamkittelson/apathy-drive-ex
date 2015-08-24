defmodule ApathyDrive.Mobile do
  alias ApathyDrive.Mobile
  alias ApathyDrive.Repo
  use GenServer
  import Systems.Text

  defstruct spirit: nil,
            socket: nil,
            hp: 10,
            max_hp: 10,
            description: "Some temporary description.",
            mana: 5,
            effects: %{},
            pid: nil,
            room_id: nil,
            alignment: nil,
            name: nil,
            keywords: [],
            enter_message: "{{name}} enters from {{direction}}.",
            exit_message: "{{name}} leaves {{direction}}.",
            gender: nil,
            greeting: nil,
            abilities: [],
            level: nil

  def start_link(state \\ %{}, opts \\ []) do
    GenServer.start_link(__MODULE__, Map.merge(%Mobile{}, state), opts)
  end

  def data_for_who_list(pid) do
    GenServer.call(pid, :data_for_who_list)
  end

  def ability_list(pid) do
    GenServer.call(pid, :ability_list)
  end

  def greeting(pid) do
    GenServer.call(pid, :greeting)
  end

  def look_name(pid) do
    GenServer.call(pid, :look_name)
  end

  def get_look_data(pid) do
    GenServer.call(pid, :look_data)
  end

  def match_data(pid) do
    GenServer.call(pid, :match_data)
  end

  def room_id(pid) do
    GenServer.call(pid, :room_id)
  end

  def name(pid) do
    GenServer.call(pid, :name)
  end

  def enter_message(pid) do
    GenServer.call(pid, :enter_message)
  end

  def exit_message(pid) do
    GenServer.call(pid, :exit_message)
  end

  def value(pid) do
    GenServer.call(pid, :value)
  end

  def alignment_color(%{alignment: "evil"}),    do: "magenta"
  def alignment_color(%{alignment: "good"}),    do: "white"
  def alignment_color(%{alignment: "neutral"}), do: "dark-cyan"

  def blind?(mobile) do
    GenServer.call(mobile, :blind?)
  end

  def held(mobile) when is_pid(mobile) do
    GenServer.call(mobile, :held?)
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

  def confused(mobile) when is_pid(mobile) do
    GenServer.call(mobile, :confused?)
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

  def send_scroll(mobile, message) when is_pid(mobile) do
    send mobile, {:send_scroll, message}
  end
  def send_scroll(%Mobile{socket: nil}, _html),  do: nil
  def send_scroll(%Mobile{socket: socket}, html) do
    send(socket, {:scroll, html})
  end

  def init(%Mobile{spirit: nil} = mobile) do
    mobile =
      mobile
      |> Map.put(:pid, self)

      ApathyDrive.PubSub.subscribe(self, "rooms:#{mobile.room_id}:mobiles")
      ApathyDrive.PubSub.subscribe(self, "rooms:#{mobile.room_id}:spawned_monsters")

    {:ok, mobile}
  end
  def init(%Mobile{spirit: spirit_id, socket: socket} = mobile) do
    spirit = Repo.get(Spirit, spirit_id)
             |> Repo.preload(:class)

    ApathyDrive.PubSub.subscribe(self, "spirits:online")
    ApathyDrive.PubSub.subscribe(self, "spirits:#{spirit.id}")
    ApathyDrive.PubSub.subscribe(socket, "spirits:#{spirit.id}:socket")

    mobile =
      mobile
      |> Map.put(:spirit, spirit)
      |> Map.put(:pid, self)
      |> Map.put(:room_id, spirit.room_id)
      |> Map.put(:alignment, ApathyDrive.Class.alignment(spirit.class_id))
      |> Map.put(:name, spirit.name)
      |> Map.put(:abilities, spirit.class.abilities)
      |> Map.put(:level, spirit.level)

    ApathyDrive.PubSub.subscribe(self, "rooms:#{mobile.room_id}:mobiles")

    {:ok, mobile}
  end

  def handle_call(:data_for_who_list, _from, mobile) do
    data = %{name: mobile.spirit.name, possessing: "", class: mobile.spirit.class.name, alignment: mobile.spirit.class.alignment}

    {:reply, data, mobile}
  end

  def handle_call(:ability_list, _from, mobile) do
    abilities =
      mobile.abilities
      |> Enum.reject(&(Map.get(&1, "command") == nil))
      |> Enum.uniq(&(Map.get(&1, "command")))
      |> Enum.sort_by(&(Map.get(&1, "level")))

    {:reply, abilities, mobile}
  end

  def handle_call(:room_id, _from, mobile) do
    {:reply, mobile.room_id, mobile}
  end

  def handle_call(:greeting, _from, mobile) do
    {:reply, mobile.greeting, mobile}
  end

  def handle_call(:look_name, _from, mobile) do
    {:reply, "<span class='#{alignment_color(mobile)}'>#{mobile.name}</span>", mobile}
  end

  def handle_call(:look_data, _from, mobile) do
    hp_percentage = round(100 * (mobile.hp / mobile.max_hp))

    hp_description = case hp_percentage do
      _ when hp_percentage >= 100 ->
        "unwounded"
      _ when hp_percentage >= 90 ->
        "slightly wounded"
      _ when hp_percentage >= 60 ->
        "moderately wounded"
      _ when hp_percentage >= 40 ->
        "heavily wounded"
      _ when hp_percentage >= 20 ->
        "severely wounded"
      _ when hp_percentage >= 10 ->
        "critically wounded"
      _ ->
        "very critically wounded"
    end

    hp_description =
      "{{target:He/She/It}} appears to be #{hp_description}."
      |> interpolate(%{"target" => mobile})

    data = %{
      name: mobile.name,
      description: mobile.description,
      hp_description: hp_description
    }

    {:reply, data, mobile}
  end

  def handle_call(:match_data, _from, mobile) do
    {:reply, %{name: mobile.name, keywords: mobile.keywords}, mobile}
  end

  def handle_call(:name, _from, mobile) do
    {:reply, mobile.name, mobile}
  end

  def handle_call(:blind?, _from, mobile) do
    blind =
      mobile.effects
      |> Map.values
      |> Enum.any?(&(Map.has_key?(&1, "blinded")))

    {:reply, blind, mobile}
  end

  def handle_call(:confused?, _from, mobile) do
    {:reply, confused(mobile), mobile}
  end

  def handle_call(:held?, _from, mobile) do
    {:reply, held(mobile), mobile}
  end

  def handle_call(:enter_message, _from, mobile) do
    {:reply, mobile.enter_message, mobile}
  end

  def handle_call(:exit_message, _from, mobile) do
    {:reply, mobile.exit_message, mobile}
  end

  def handle_call(:value, _from, mobile) do
    {:reply, mobile, mobile}
  end

  def handle_info(:display_prompt, %Mobile{socket: _socket} = mobile) do
    display_prompt(mobile)

    {:noreply, mobile}
  end

  def handle_info({:send_scroll, message}, mobile) do
    send_scroll(mobile, message)

    {:noreply, mobile}
  end

  def handle_info({:move_to, room_id}, mobile) do
    ApathyDrive.PubSub.unsubscribe(self, "rooms:#{mobile.room_id}:mobiles")
    mobile = Map.put(mobile, :room_id, room_id)
    ApathyDrive.PubSub.subscribe(self, "rooms:#{room_id}:mobiles")

    if mobile.spirit do
      mobile = put_in(mobile.spirit.room_id, mobile.room_id)
      Spirit.save(mobile.spirit)
      {:noreply, mobile}
    else
      {:noreply, mobile}
    end
  end

  def handle_info(%Phoenix.Socket.Broadcast{}, %Mobile{socket: nil} = mobile) do
    {:noreply, mobile}
  end
  def handle_info(%Phoenix.Socket.Broadcast{} = message, %Mobile{socket: socket} = mobile) do
    send(socket, message)

    {:noreply, mobile}
  end

  def display_prompt(%Mobile{socket: socket} = mobile) do
    ApathyDrive.PubSub.broadcast_from! socket, "spirits:#{mobile.spirit.id}:socket", :go_home

    send(socket, {:disable_element, "#prompt"})
    send(socket, {:disable_element, "#command"})
    send(socket, {:scroll, "<p><span id='prompt'>#{prompt(mobile)}</span><input id='command' size='50' class='prompt'></input></p>"})
    send(socket, {:focus_element, "#command"})
    send(socket, :up)
  end

  # def update(%Spirit{} = spirit) do
  #   spirit
  #   |> Spirit.send_update_prompt(prompt(spirit))
  # end
  #
  # def update(%Monster{} = monster) do
  #   Monster.send_update_prompt(monster, prompt(monster))
  # end
  #
  # def prompt(%Spirit{} = spirit) do
  #   "[Level=#{spirit.level}/MA=#{spirit.mana}]:"
  # end
  #
  def prompt(%Mobile{} = mobile) do
    "[HP=#{mobile.hp}/MA=#{mobile.mana}]:"
  end

end