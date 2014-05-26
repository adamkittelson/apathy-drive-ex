Code.ensure_compiled(Account)

defmodule Components.Login do
  use GenEvent.Behaviour

  ### Public API
  def value(player) do
    :gen_event.call(player, Components.Login, :value)
  end

  def get_step(player) do
    :gen_event.call(player, Components.Login, :get_step)
  end

  def intro(player) do
    Entity.notify(player, {:intro})
    Players.send_message(player, ["scroll", "<p>Please enter your email address to log in or 'new' to create a new account: <input id='email' class='prompt'></input></p>"])
    Players.send_message(player, ["focus", "#email"])
  end

  def create_account_request_email(player) do
    Entity.notify(player, :create_account_request_email)
    Players.send_message(player, ["scroll", "<p>Please enter the email address you would like to use: <input id='email' class='prompt'></input></p>"])
    Players.send_message(player, ["focus", "#email"])
  end

  def sign_in_get_account(player, email) do
    Entity.notify(player, {:sign_in_set_email, email})
    Players.send_message(player, ["scroll", "<p>Please enter your password: <input id='password' type='password' class='prompt'></input></p>"])
    Players.send_message(player, ["focus", "#password"])
  end

  def sign_in_check_password(player, password) do
    email = :gen_event.call(player, Components.Login, :get_email)
    account = Account.find(email, password)
    if account do
      display_character_select(player, account)
    else
      Players.send_message(player, ["scroll", "<p>Invalid username/password!</p>"])
      intro(player)
    end
  end

  def display_character_select(player, account) do
    Players.send_message(player, ["clear scroll"])
    Entity.notify(player, {:sign_in_set_account, account})
    Players.send_message(player, ["scroll", "\n<p><span class='dark-yellow underline'>Characters</span></p>\n\n"])
    Enum.each(Characters.for_account(account), fn(character) ->
      name  = Components.Name.get_name(character)
      race  = Components.Race.value(character)  |> Components.Name.get_name
      class = Components.Class.value(character) |> Components.Name.get_name
      Players.send_message(player, ["scroll", "<p><span class='white'>#{name}</span> <span class='dark-green'>:</span> <span class='dark-yellow'>#{race} #{class}</span></p>"])
    end)
    Players.send_message(player, ["scroll", "\n\n\n\n<p><span class='dark-red'>N</span> <span class='dark-green'>:</span> <span class='dark-yellow'>New Character</span></p>"])
    Players.send_message(player, ["scroll", "<p><span class='dark-yellow'>Please enter your selection:</span> <input id='character' class='prompt'></input></p>"])
    Players.send_message(player, ["focus", "#character"])
  end

  def display_race_select(player) do
    Entity.notify(player, :create_character_request_race)
    Players.send_message(player, ["scroll", "<p><span class='white'>Please choose a race from the following list:</span></p>"])
    Enum.sort(Races.all, &(Components.Number.get_number(&1) < Components.Number.get_number(&2)))
    |> Enum.each fn(race) ->
      Players.send_message(player, ["scroll", "<p><span class='dark-grey'>[</span><span class='white'>#{Components.Number.get_number(race)}</span><span class='dark-grey'>]</span> #{Components.Name.get_name(race)}</p>"])
    end
    prompt_for_race(player)
  end

  def prompt_for_race(player) do
    Players.send_message(player, ["scroll", "\n\n<p><span class='dark-green'>Please choose your race [ 'help &lt;race&gt;' for more info ]: </span><input id='race' class='prompt'></input></p>"])
    Players.send_message(player, ["focus", "#race"])
  end

  def prompt_for_class(player) do
    Players.send_message(player, ["scroll", "\n\n<p><span class='dark-green'>Please choose your class [ 'help &lt;class&gt;' for more info ]: </span><input id='class' class='prompt'></input></p>"])
    Players.send_message(player, ["focus", "#class"])
  end

  def display_class_select(player) do
    Entity.notify(player, :create_character_request_class)
    Players.send_message(player, ["scroll", "<p><span class='white'>Please choose a class from the following list:</span></p>"])
    Enum.sort(Classes.all, &(Components.Number.get_number(&1) < Components.Number.get_number(&2)))
    |> Enum.each fn(class) ->
      Players.send_message(player, ["scroll", "<p><span class='dark-grey'>[</span><span class='white'>#{Components.Number.get_number(class)}</span><span class='dark-grey'>]</span> #{Components.Name.get_name(class)}</p>"])
    end
    prompt_for_class(player)
  end

  def create_character_set_race(player, race_number) do
    if Regex.match?(~r/^\d+$/, race_number) do
      {number, _} = Integer.parse(race_number)
      race = Races.find_by_number(number)
      if race do
        Entity.notify(player, {:create_character_set_race, race})
        display_class_select(player)
      else
        Players.send_message(player, ["scroll", "There is no race with that number."])
      end
    else
      Components.Login.display_race_select(player)
    end
  end

  def create_character_set_class(player, class_number) do
    if Regex.match?(~r/^\d+$/, class_number) do
      {number, _} = Integer.parse(class_number)
      class = Classes.find_by_number(number)
      if class do
        race = get_race(player)
        {:ok, character} = Entity.init
        Entity.add_component(character, Components.Agility,   Components.Agility.value(race))
        Entity.add_component(character, Components.Charm,     Components.Charm.value(race))
        Entity.add_component(character, Components.Health,    Components.Health.value(race))
        Entity.add_component(character, Components.Intellect, Components.Intellect.value(race))
        Entity.add_component(character, Components.Strength,  Components.Strength.value(race))
        Entity.add_component(character, Components.Willpower, Components.Willpower.value(race))
        Entity.add_component(character, Components.CP, 100)
        Entity.add_component(character, Components.Class, class)
        Entity.add_component(character, Components.Race, race)
        Entity.add_component(character, Components.Name, "")
        Entity.add_component(character, Components.Gender, nil)
        Entity.add_component(character, Components.EyeColor, nil)
        Entity.add_component(character, Components.HairColor, nil)
        Entity.add_component(character, Components.HairLength, nil)
        Entity.add_component(character, Components.AccountID, Components.Login.get_account(player).id)
        Entity.add_component(character, Components.HPRolls, [Components.MaxHPPerLevel.value(class)])
        Entity.add_component(character, Components.Level, 1)
        Entity.add_component(character, Components.HP, Systems.HP.max_hp(character))
        Entity.add_component(character, Components.Online, false)
        Entity.add_component(character, Components.Player, player)

        Systems.Training.train_stats(player, character)
      else
        Players.send_message(player, ["scroll", "There is no class with that number."])
      end
    else
      Components.Login.display_class_select(player)
    end
  end

  def create_account_set_email(player, email) do
    account = Account.find(email)
    if account do
      sign_in_get_account(player, email)
    else
      Entity.notify(player, {:create_account_set_email, email})
      Players.send_message(player, ["scroll", "<p>Please enter the password you would like to use: <input id='password' type='password' class='prompt'></input></p>"])
      Players.send_message(player, ["focus", "#password"])
    end
  end

  def create_account_set_password(player, password) do
    Entity.notify(player, {:create_account_set_password, password})
    Players.send_message(player, ["scroll", "<p>Please confirm your new password: <input id='password-confirmation' type='password' class='prompt'></input></p>"])
    Players.send_message(player, ["focus", "#password-confirmation"])
  end

  def create_account_finish(player, password) do
    if password_confirmed?(player, password) do
      Players.send_message(player, ["scroll", "<p>Welcome!</p>"])
      account = %Account{ :email    => :gen_event.call(player, Components.Login, :get_email),
                          :password =>  "#{:gen_event.call(player, Components.Login, :get_password)}",
                          :salt     =>  "#{:gen_event.call(player, Components.Login, :get_salt)}"
      }
      account = Repo.insert(account)
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

  def get_class(player) do
    :gen_event.call(player, Components.Login, :get_class)
  end

  def get_race(player) do
    :gen_event.call(player, Components.Login, :get_race)
  end

  def get_character(player) do
    :gen_event.call(player, Components.Login, :get_character)
  end

  def get_cp(player) do
    :gen_event.call(player, Components.Login, :get_cp)
  end

  def get_stat(player, stat_name) do
    :gen_event.call(player, Components.Login, {:get_stat, stat_name})
  end

  def set_stat(player, stat_name, stat) do
    Entity.notify(player, {:set_stat, stat_name, stat})
  end

  def get_hair_length(player) do
    :gen_event.call(player, Components.Login, :get_hair_length)
  end

  def set_hair_length(player, hair_length) do
    Entity.notify(player, {:set_hair_length, hair_length})
  end

  def get_hair_color(player) do
    :gen_event.call(player, Components.Login, :get_hair_color)
  end

  def value(entity, new_value) do
    Entity.notify(entity, {:set_login, new_value})
  end

  def set_hair_color(player, hair_color) do
    Entity.notify(player, {:set_hair_color, hair_color})
  end

  def get_eye_color(player) do
    :gen_event.call(player, Components.Login, :get_eye_color)
  end

  def set_eye_color(player, eye_color) do
    Entity.notify(player, {:set_eye_color, eye_color})
  end

  def get_gender(player) do
    :gen_event.call(player, Components.Login, :get_gender)
  end

  def set_gender(player, gender) do
    Entity.notify(player, {:set_gender, gender})
  end

  def get_name(player) do
    :gen_event.call(player, Components.Login, :get_name)
  end

  def get_last_name(player) do
    :gen_event.call(player, Components.Login, :get_last_name)
  end

  def get_account(player) do
    :gen_event.call(player, Components.Login, :get_account)
  end

  def set_name(player, name) do
    Entity.notify(player, {:set_name, name})
  end

  def set_last_name(player, name) do
    Entity.notify(player, {:set_last_name, name})
  end

  def set_cp(player, cp) do
    Entity.notify(player, {:set_cp, cp})
  end

  def select_character(player, character_name) do
    account = get_account(player)
    character = Characters.find_by_account_and_name(account, character_name)
    if character do
      login(player, character)
    else
      Players.send_message(player, ["scroll", "<p><span class='red'>You have no characters with that name.</span></p>"])
    end
  end

  def login(player, character) do
    Entity.notify(player, {:login, character})
    Components.Online.value(character, true)
    Components.Player.value(character, player)
    Components.Player.send_message(character, ["clear scroll"])
    Systems.Room.display_room_in_scroll(character, Components.CurrentRoom.get_current_room(character))
    Systems.Command.display_prompt(character)
  end

  def serialize(_entity) do
    nil
  end

  ### GenEvent API
  def init(state) do
    {:ok, state}
  end

  def handle_call(:value, state) do
    {:ok, state, state}
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

  def handle_call(:get_race, state) do
    {:ok, state[:race], state}
  end

  def handle_call(:get_character, state) do
    {:ok, state[:character], state}
  end

  def handle_call(:get_cp, state) do
    {:ok, state[:stats][:cp], state}
  end

  def handle_call({:get_stat, stat_name}, state) do
    {:ok, state[:stats][stat_name], state}
  end

  def handle_call(:get_class, state) do
    {:ok, state[:class], state}
  end

  def handle_call(:get_hair_length, state) do
    {:ok, state[:hair_length], state}
  end

  def handle_call(:get_hair_color, state) do
    {:ok, state[:hair_color], state}
  end

  def handle_call(:get_eye_color, state) do
    {:ok, state[:eye_color], state}
  end

  def handle_call(:get_gender, state) do
    {:ok, state[:gender], state}
  end

  def handle_call(:get_name, state) do
    {:ok, state[:name], state}
  end

  def handle_call(:get_last_name, state) do
    {:ok, state[:last_name], state}
  end

  def handle_call(:get_account, state) do
    {:ok, state[:account], state}
  end

  def handle_event({:set_login, new_value}, _value) do
    {:ok, new_value }
  end

  def handle_event({:set_stat, stat_name, stat}, state) do
    stats = Keyword.put(state[:stats], stat_name, stat)
    {:ok, Keyword.put(state, :stats, stats)}
  end

  def handle_event({:set_cp, cp}, state) do
    stats = Keyword.put(state[:stats], :cp, cp)
    {:ok, Keyword.put(state, :stats, stats)}
  end

  def handle_event({:set_hair_length, hair_length}, state) do
    {:ok, Keyword.put(state, :hair_length, hair_length)}
  end

  def handle_event({:set_hair_color, hair_color}, state) do
    {:ok, Keyword.put(state, :hair_color, hair_color)}
  end

  def handle_event({:set_eye_color, eye_color}, state) do
    {:ok, Keyword.put(state, :eye_color, eye_color)}
  end

  def handle_event({:set_gender, gender}, state) do
    {:ok, Keyword.put(state, :gender, gender)}
  end

  def handle_event({:set_name, name}, state) do
    {:ok, Keyword.put(state, :name, name)}
  end

  def handle_event({:set_last_name, name}, state) do
    {:ok, Keyword.put(state, :last_name, name)}
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

  def handle_event({:training, character, stats}, _state) do
    {:ok, [step: "training", character: character, stats: stats]}
  end

  def handle_event({:login, character}, _state) do
    {:ok, [step: "playing", character: character]}
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
