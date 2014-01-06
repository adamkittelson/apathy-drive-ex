defmodule Components.Login do
  use GenEvent.Behaviour

  ### Public API
  def get_step(player) do
    :gen_event.call(player, Components.Login, :get_step)
  end

  def intro(player) do
    ApathyDrive.Entity.notify(player, {:intro})
    Players.send_message(player, ["scroll", "Please enter your email address to log in or 'new' to create a new account."])
  end

  def create_account_request_email(player) do
    ApathyDrive.Entity.notify(player, :create_account_request_email)
    Players.send_message(player, ["scroll", "Please enter the email address you would like to use."])
  end

  def sign_in_get_account(player, email) do
    ApathyDrive.Entity.notify(player, {:sign_in_set_email, email})
    Players.send_message(player, ["scroll", "Please enter your password."])
  end

  def sign_in_check_password(player, password) do
    email = :gen_event.call(player, Components.Login, :get_email)
    account = ApathyDrive.Account.find(email, password)
    if account do
      Players.send_message(player, ["scroll", "Welcome back!"])
      display_character_select(player, account)
    else
      Players.send_message(player, ["scroll", "Invalid username/password!"])
      intro(player)
    end
  end

  def display_character_select(player, account) do
    ApathyDrive.Entity.notify(player, {:sign_in_set_account, account})
    Players.send_message(player, ["scroll", "<span class='dark-yellow underline'>Characters</span>"])
    Players.send_message(player, ["scroll", "<br><span class='dark-red'>N</span> <span class='dark-green'>:</span> <span class='dark-yellow'>New Character</span>"])
  end

  def create_account_set_email(player, email) do
    account = ApathyDrive.Account.find(email)
    if account do
      sign_in_get_account(player, email)
    else
      ApathyDrive.Entity.notify(player, {:create_account_set_email, email})
      Players.send_message(player, ["scroll", "Please enter the password you would like to use."])
    end
  end

  def create_account_set_password(player, password) do
    ApathyDrive.Entity.notify(player, {:create_account_set_password, password})
    Players.send_message(player, ["scroll", "Please confirm your new password."])
  end

  def create_account_finish(player, password) do
    if password_confirmed?(player, password) do
      Players.send_message(player, ["scroll", "Welcome!"])
      account = ApathyDrive.Account.new(email:     :gen_event.call(player, Components.Login, :get_email),
                                        password:  "#{:gen_event.call(player, Components.Login, :get_password)}",
                                        salt:      "#{:gen_event.call(player, Components.Login, :get_salt)}"
      )
      Repo.create account
      display_character_select(player, account)
    else
      Players.send_message(player, ["scroll", "Passwords did not match."])
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
