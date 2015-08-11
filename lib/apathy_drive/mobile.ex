defmodule ApathyDrive.Mobile do
  alias ApathyDrive.Mobile
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
            keywords: []

  def start_link(state \\ %{}, opts \\ []) do
    GenServer.start_link(__MODULE__, Map.merge(%Mobile{}, state), opts)
  end

  def data_for_who_list(pid) do
    GenServer.call(pid, :data_for_who_list)
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

  def alignment_color(%{alignment: "evil"}),    do: "magenta"
  def alignment_color(%{alignment: "good"}),    do: "white"
  def alignment_color(%{alignment: "neutral"}), do: "dark-cyan"

  def blind?(%Mobile{effects: effects}) do
    effects
    |> Map.values
    |> Enum.any?(&(Map.has_key?(&1, "blinded")))
  end

  def send_scroll(%Mobile{socket: nil} = mobile, _html), do: mobile
  def send_scroll(%Mobile{socket: socket} = mobile, html) do
    send(socket, {:scroll, html})
    mobile
  end

  def init(%Mobile{spirit: nil} = state) do
    {:ok, state}
  end
  def init(%Mobile{spirit: spirit, socket: socket} = mobile) do
    ApathyDrive.PubSub.subscribe(self, "spirits:online")
    ApathyDrive.PubSub.subscribe(self, "spirits:#{spirit.id}")
    ApathyDrive.PubSub.subscribe(socket, "spirits:#{spirit.id}:socket")

    mobile =
      mobile
      |> Map.put(:pid, self)
      |> Map.put(:room_id, spirit.room_id)
      |> Map.put(:alignment, spirit.alignment)
      |> Map.put(:name, spirit.name)

    ApathyDrive.PubSub.subscribe(self, "rooms:#{mobile.room_id}:mobiles")

    {:ok, mobile}
  end

  def handle_call(:data_for_who_list, _from, mobile) do
    data = %{name: mobile.spirit.name, possessing: "", faction: mobile.spirit.faction, alignment: mobile.spirit.alignment}

    {:reply, data, mobile}
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

  def handle_info({:execute_command, command, arguments}, %Mobile{} = mobile) do
    ApathyDrive.Command.execute(mobile, command, arguments)
    {:noreply, mobile}
  end

  def handle_info(:display_prompt, %Mobile{socket: _socket} = mobile) do
    display_prompt(mobile)

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