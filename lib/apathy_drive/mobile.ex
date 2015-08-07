defmodule ApathyDrive.Mobile do
  alias ApathyDrive.Mobile
  use GenServer

  defstruct spirit: nil, socket: nil, hp: 10, mana: 5

  def start_link(state \\ %{}, opts \\ []) do
    GenServer.start_link(__MODULE__, Map.merge(%Mobile{}, state), opts)
  end

  def data_for_who_list(pid) do
    GenServer.call(pid, :data_for_who_list)
  end

  def alignment_color(%{alignment: "evil"}),    do: "magenta"
  def alignment_color(%{alignment: "good"}),    do: "white"
  def alignment_color(%{alignment: "neutral"}), do: "dark-cyan"

  def init(%Mobile{spirit: nil} = state) do
    {:ok, state}
  end
  def init(%Mobile{spirit: spirit, socket: socket} = state) do
    ApathyDrive.PubSub.subscribe(self, "spirits:online")
    ApathyDrive.PubSub.subscribe(self, "spirits:#{spirit.id}")
    ApathyDrive.PubSub.subscribe(socket, "spirits:#{spirit.id}:socket")

    {:ok, state}
  end

  def handle_call(:data_for_who_list, _from, mobile) do
    data = %{name: mobile.spirit.name, possessing: "", faction: "Demon", alignment: "evil"}

    {:reply, data, mobile}
  end

  def handle_info({:execute_command, command, arguments}, %Mobile{} = mobile) do
    ApathyDrive.Command.execute(mobile, command, arguments)
    {:noreply, mobile}
  end

  def handle_info(:display_prompt, %Mobile{socket: _socket} = mobile) do
    display_prompt(mobile)

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