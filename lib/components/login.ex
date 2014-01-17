defmodule Components.Login do
  use GenEvent.Behaviour

  ### Public API
  def get_step(player) do
    :gen_event.call(player, Components.Login, :get_step)
  end

  def intro(player) do
    ApathyDrive.Entity.notify(player, {:intro})
    Players.send_message(player, ["scroll", "<p>Please enter your email address to log in or 'new' to create a new account.</p>"])
  end

  def create_account_request_email(player) do
    ApathyDrive.Entity.notify(player, :create_account_request_email)
    Players.send_message(player, ["scroll", "<p>Please enter the email address you would like to use.</p>"])
  end

  def sign_in_get_account(player, email) do
    ApathyDrive.Entity.notify(player, {:sign_in_set_email, email})
    Players.send_message(player, ["scroll", "<p>Please enter your password.</p>"])
  end

  def sign_in_check_password(player, password) do
    email = :gen_event.call(player, Components.Login, :get_email)
    account = ApathyDrive.Account.find(email, password)
    if account do
      Players.send_message(player, ["scroll", "<p>Welcome back!</p>"])
      display_character_select(player, account)
    else
      Players.send_message(player, ["scroll", "<p>Invalid username/password!</p>"])
      intro(player)
    end
  end

  def display_character_select(player, account) do
    ApathyDrive.Entity.notify(player, {:sign_in_set_account, account})
    Players.send_message(player, ["scroll", "<p><span class='dark-yellow underline'>Characters</span></p>"])
    Players.send_message(player, ["scroll", "\n\n\n\n<p><span class='dark-red'>N</span> <span class='dark-green'>:</span> <span class='dark-yellow'>New Character</span></p>"])
  end

  def display_race_select(player) do
    ApathyDrive.Entity.notify(player, :create_character_request_race)
    Players.send_message(player, ["scroll", "<p><span class='white'>Please choose a race from the following list:</span></p>"])
    Enum.sort(Races.all, &(Components.Number.get_number(&1) < Components.Number.get_number(&2)))
    |> Enum.each fn(race) ->
      Players.send_message(player, ["scroll", "<p><span class='dark-grey'>[</span><span class='white'>#{Components.Number.get_number(race)}</span><span class='dark-grey'>]</span> #{Components.Name.get_name(race)}</p>"])
    end
    Players.send_message(player, ["scroll", "\n\n<p><span class='dark-green'>Please choose your race [ 'help &lt;race&gt;' for more info ] :</span></p>"])
  end

  def display_class_select(player) do
    ApathyDrive.Entity.notify(player, :create_character_request_class)
    Players.send_message(player, ["scroll", "<p><span class='white'>Please choose a class from the following list:</span></p>"])
    Enum.sort(Classes.all, &(Components.Number.get_number(&1) < Components.Number.get_number(&2)))
    |> Enum.each fn(class) ->
      Players.send_message(player, ["scroll", "<p><span class='dark-grey'>[</span><span class='white'>#{Components.Number.get_number(class)}</span><span class='dark-grey'>]</span> #{Components.Name.get_name(class)}</p>"])
    end
    Players.send_message(player, ["scroll", "\n\n<p><span class='dark-green'>Please choose your class [ 'help &lt;class&gt;' for more info ] :</span></p>"])
  end

  def create_character_set_race(player, race_number) do
    if Regex.match?(%r/^\d+$/, race_number) do
      {number, _} = Integer.parse(race_number)
      race = Races.find_by_number(number)
      if race do
        ApathyDrive.Entity.notify(player, {:create_character_set_race, race})
        display_class_select(player)
      else
        Players.send_message(player, ["scroll", "There is no race with that number."])
      end
    else
      Components.Login.display_race_select(player)
    end
  end

  def create_account_set_email(player, email) do
    account = ApathyDrive.Account.find(email)
    if account do
      sign_in_get_account(player, email)
    else
      ApathyDrive.Entity.notify(player, {:create_account_set_email, email})
      Players.send_message(player, ["scroll", "<p>Please enter the password you would like to use.</p>"])
    end
  end

  def create_account_set_password(player, password) do
    ApathyDrive.Entity.notify(player, {:create_account_set_password, password})
    Players.send_message(player, ["scroll", "<p>Please confirm your new password.</p>"])
  end

  def create_account_finish(player, password) do
    if password_confirmed?(player, password) do
      Players.send_message(player, ["scroll", "<p>Welcome!</p>"])
      account = ApathyDrive.Account.new(email:     :gen_event.call(player, Components.Login, :get_email),
                                        password:  "#{:gen_event.call(player, Components.Login, :get_password)}",
                                        salt:      "#{:gen_event.call(player, Components.Login, :get_salt)}"
      )
      Repo.create account
      display_character_select(player, account)
    else
      Players.send_message(player, ["scroll", "<p>Passwords did not match.</p>"])
      email = :gen_event.call(player, Components.Login, :get_email)
      create_account_set_email(player, email)
    end
  end

  def password_confirmed?(player, password_confirmation) do
    password = :gen_event.call(player, Components.Login, :get_password)
    salt     = :gen_event.call(player, Components.Login, :get_salt)
    {:ok, password} == :bcrypt.hashpw(password_confirmation, salt)
  end

  ### GenEvent API
  def init(state) do
    {:ok, state}
  end

  def handle_call(:get_step, state) do
    {:ok, state[:step], state}
  end

  def handle_call(:get_email, state) do
    {:ok, state[:email], state}
  end

  def handle_call(:get_password, state) do
    {:ok, state[:password], state}
  end

  def handle_call(:get_salt, state) do
    {:ok, state[:salt], state}
  end

  def handle_call(:get_account, state) do
    {:ok, state[:account], state}
  end

  def handle_event({:intro}, _state) do
    {:ok, [step: "intro"]}
  end

  def handle_event({:sign_in_set_email, email}, _state) do
    {:ok, [step: "sign_in_check_password", email: email]}
  end

  def handle_event({:sign_in_set_account, account}, _state) do
    {:ok, [step: "character_select", account: account]}
  end

  def handle_event(:create_character_request_race, state) do
    {:ok, [step: "create_character_request_race", account: state[:account]]}
  end

  def handle_event({:create_character_set_race, race}, state) do
    {:ok, [step: "create_character_request_class", race: race, account: state[:account]]}
  end

  def handle_event(:create_account_request_email, _state) do
    {:ok, [step: "create_account_request_email"]}
  end

  def handle_event({:create_account_set_email, email}, _state) do
    {:ok, salt} = :bcrypt.gen_salt
    {:ok, [step: "create_account_request_password", email: email, salt: salt]}
  end

  def handle_event({:create_account_set_password, password}, state) do
    {:ok, password} = :bcrypt.hashpw(password, state[:salt])
    {:ok, Keyword.merge(state, [step: "create_account_confirm_password", password: password])}
  end

  def handle_event(_, state) do
    {:ok, state}
  end
end
