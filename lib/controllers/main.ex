defmodule ApathyDrive.Main do

  use Weber.Controller

  layout false
  def action([], _conn) do
    {:render, [], []}
  end

  def websocket_init(pid, conn) do
    IO.puts "Connected! pid: #{inspect pid}"
    room_pid = :global.whereis_name(:"82325")
    room_info = ["room", [
      name: ApathyDrive.NameComponent.get_name(room_pid),
      description: ApathyDrive.DescriptionComponent.get_description(room_pid),
      exits: Enum.map(ApathyDrive.ExitsComponent.get_exits(room_pid), fn (exit_pid) ->
        ApathyDrive.DirectionComponent.get_direction(exit_pid)
      end) ]]
      
    message = JSON.generate(room_info)
    IO.puts message
    pid <- message
  end

  def websocket_message(pid, message, conn) do
    [{event, [message]}] = JSON.parse(message)
    IO.puts "Message! Pid: #{inspect pid}, Event: #{inspect event} Message: #{inspect message}"
    {_label, text} = message
    case event do
      "command" ->
        pid <- JSON.generate(["scroll", "You said #{text}."])
    end
  end

  def websocket_terminate(pid, conn) do
    IO.puts "Terminated! Pid: #{inspect pid}"
  end

end
