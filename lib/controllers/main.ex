defmodule ApathyDrive.Main do

  use Weber.Controller

  layout false
  def action([], _conn) do
    {:render, [], []}
  end

  def websocket_init(pid, _conn) do
    Players.connected(pid)
  end

  def websocket_message(pid, message, _conn) do
    [{event, message}]    = JSON.parse(message)
    [command | arguments] = String.split(message)
    player                = Players.find_by_connection(pid)
    case event do
      "command" ->
        Systems.Command.execute(player, command, arguments)
      "email" ->
        Players.send_message(player, ["disable", "##{event}"])
        case Components.Login.get_step(player) do
          "create_account_request_email" ->
            Components.Login.create_account_set_email(player, command)
          "intro" ->
            case command do
              "new" ->
                Components.Login.create_account_request_email(player)
              _other ->
                Components.Login.sign_in_get_account(player, command)
            end
        end
      "password" ->
        Players.send_message(player, ["disable", "##{event}"])
        case Components.Login.get_step(player) do
          "sign_in_check_password" ->
            Components.Login.sign_in_check_password(player, command)
          "create_account_request_password" ->
            Components.Login.create_account_set_password(player, command)
        end
      "password-confirmation" ->
        Players.send_message(player, ["disable", "##{event}"])
        Components.Login.create_account_finish(player, command)
      "character" ->
        case command do
          "N" ->
            Players.send_message(player, ["disable", "##{event}"])
            Components.Login.display_race_select(player)
          "n" ->
            Players.send_message(player, ["disable", "##{event}"])
            Components.Login.display_race_select(player)
          _character_name ->
            Components.Login.select_character(player, command)
        end
      "race" ->
        Players.send_message(player, ["disable", "##{event}"])
        case command do
          "help" ->
            Systems.Command.help(player, arguments)
            Components.Login.prompt_for_race(player)
          _other -> Components.Login.create_character_set_race(player, command)
        end
      "class" ->
        Players.send_message(player, ["disable", "##{event}"])
        case command do
          "help" ->
            Systems.Command.help(player, arguments)
            Components.Login.prompt_for_class(player)
          _other -> Components.Login.create_character_set_class(player, command)
        end
      "strength" ->
        if Components.Login.get_step(player) == "training" do
          Systems.Training.set_stat(player, "strength", command)
        end
      "agility" ->
        if Components.Login.get_step(player) == "training" do
          Systems.Training.set_stat(player, "agility", command)
        end
      "intellect" ->
        if Components.Login.get_step(player) == "training" do
          Systems.Training.set_stat(player, "intellect", command)
        end
      "willpower" ->
        if Components.Login.get_step(player) == "training" do
          Systems.Training.set_stat(player, "willpower", command)
        end
      "health" ->
        if Components.Login.get_step(player) == "training" do
          Systems.Training.set_stat(player, "health", command)
        end
      "charm" ->
        if Components.Login.get_step(player) == "training" do
          Systems.Training.set_stat(player, "charm", command)
        end
      "cycle" ->
        if Components.Login.get_step(player) == "training" do
          Systems.Training.cycle_options(player, command)
        end
       "gender" ->
         if Components.Login.get_step(player) == "training" do
           Systems.Training.validate_attribute(player, "gender", command)
         end
       "hair_color" ->
          if Components.Login.get_step(player) == "training" do
            Systems.Training.validate_attribute(player, "hair_color", command)
          end
       "hair_length" ->
          if Components.Login.get_step(player) == "training" do
            Systems.Training.validate_attribute(player, "hair_length", command)
          end
       "eye_color" ->
          if Components.Login.get_step(player) == "training" do
            Systems.Training.validate_attribute(player, "eye_color", command)
          end
       "first-name" ->
          if Components.Login.get_step(player) == "training" do
            Systems.Training.validate_name(player, command)
          end
       "last-name" ->
          if Components.Login.get_step(player) == "training" do
            Systems.Training.validate_last_name(player, command)
          end
       "save" ->
          if Components.Login.get_step(player) == "training" do
            Systems.Training.finish(player)
          end
      _ ->
    end
  end

  def websocket_terminate(pid, _conn) do
    Players.disconnected(pid)
  end

end
