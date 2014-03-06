defmodule Systems.Training do

    def train_stats(player, character) do
      race       = Components.Race.value(character)
      class      = Components.Class.value(character)
      race_name  = Components.Name.get_name(race)
      class_name = Components.Name.get_name(class)

      str = Components.Strength.value(character)
      agi = Components.Agility.value(character)
      int = Components.Intellect.value(character)
      wil = Components.Willpower.value(character)
      hea = Components.Health.value(character)
      cha = Components.Charm.value(character)
      cp  = Components.CP.value(character)

      stats = [cp: cp, strength: str, agility: agi, intellect: int, willpower: wil, health: hea, charm: cha]

      ApathyDrive.Entity.notify(player, {:training, character, stats})

      max_str = max_stat(player, character, "Strength")
      max_agi = max_stat(player, character, "Agility")
      max_int = max_stat(player, character, "Intellect")
      max_wil = max_stat(player, character, "Willpower")
      max_hea = max_stat(player, character, "Health")
      max_cha = max_stat(player, character, "Charm")

      Players.send_message(player, ["clear scroll"])
      Players.send_message(player, ["scroll",
  """
  <div>   .─────────────────────────────────────.──.</div>
  <div>  /  <span class="dark-grey">Apathy</span> <span class="dark-red">Drive</span> <span class="dark-cyan">Character Creation</span>    /    \\  <span class="dark-grey">┌─</span>    <span class="magenta">Point Cost Chart</span>    <span class="dark-grey">─┐</span></div>
  <div> │                                     ├──.   │ <span class="dark-grey">│</span>                          <span class="dark-grey">│</span></div>
  <div> │ <span class="dark-red">»</span> <span class="dark-cyan">Given Name</span>   <input id="first-name" class="field" maxlength="18" size="18"></input> <span class="dark-red">«</span> │___\\_/  <span class="dark-grey">│</span> <span class="magenta">1st</span> <span class="dark-magenta">10 points:</span> <span class="magenta">1</span> <span class="dark-magenta">CP each</span> <span class="dark-grey">│</span></div>
  <div> │ <span class="dark-red">»</span> <span class="dark-cyan">Family Name</span>  <input id="last-name" class="field" maxlength="18" size="18"></input> <span class="dark-red">«</span> │        <span class="dark-grey">│</span> <span class="magenta">2nd</span> <span class="dark-magenta">10 points:</span> <span class="magenta">2</span> <span class="dark-magenta">CP each</span> <span class="dark-grey">│</span></div>
  <div> │ <span class="dark-red">»</span> <span class="dark-cyan">Race</span>         #{String.ljust(race_name, 19)}<span class="dark-red">«</span> │        <span class="dark-grey">│</span> <span class="magenta">3rd</span> <span class="dark-magenta">10 points:</span> <span class="magenta">3</span> <span class="dark-magenta">CP each</span> <span class="dark-grey">│</span></div>
  <div> │ <span class="dark-red">»</span> <span class="dark-cyan">Class</span>        #{String.ljust(class_name, 19)}<span class="dark-red">«</span> │        <span class="dark-grey">│     ... and so on ...    │</span></div>
  <div> │                                     │        <span class="dark-grey">│</span>                          <span class="dark-grey">│</span></div>
  <div> │ <span class="dark-red">»</span> <span class="dark-cyan">Strength</span>   (#{String.rjust("#{str}", 4)} to <span id="max-strength">#{String.rjust("#{max_str}", 4)}</span>)   <input id="strength" class="field" maxlength="3" size="3" value="#{str}"></input> <span class="dark-red">«</span> │        <span class="dark-grey">│</span> <span class="dark-magenta">+</span><span class="magenta">10</span> <span class="dark-magenta">to base stat:</span>  <span class="magenta">10</span> <span class="dark-magenta">CP</span> <span class="dark-grey">│</span></div>
  <div> │ <span class="dark-red">»</span> <span class="dark-cyan">Intellect</span>  (#{String.rjust("#{int}", 4)} to <span id="max-intellect">#{String.rjust("#{max_int}", 4)}</span>)   <input id="intellect" class="field" maxlength="3" size="3" value="#{int}"></input> <span class="dark-red">«</span> │ <span class="arrow"><span class="dark-grey">◀──────┤</span> <span class="dark-magenta">+</span><span class="magenta">20</span> <span class="dark-magenta">to base stat:</span>  <span class="magenta">30</span> <span class="dark-magenta">CP</span> <span class="dark-grey">│</span></span></div>
  <div> │ <span class="dark-red">»</span> <span class="dark-cyan">Willpower</span>  (#{String.rjust("#{wil}", 4)} to <span id="max-willpower">#{String.rjust("#{max_wil}", 4)}</span>)   <input id="willpower" class="field" maxlength="3" size="3" value="#{wil}"></input> <span class="dark-red">«</span> │        <span class="dark-grey">│</span> <span class="dark-magenta">+</span><span class="magenta">30</span> <span class="dark-magenta">to base stat:</span>  <span class="magenta">60</span> <span class="dark-magenta">CP</span> <span class="dark-grey">│</span></div>
  <div> │ <span class="dark-red">»</span> <span class="dark-cyan">Agility</span>    (#{String.rjust("#{agi}", 4)} to <span id="max-agility">#{String.rjust("#{max_agi}", 4)}</span>)   <input id="agility" class="field" maxlength="3" size="3" value="#{agi}"></input> <span class="dark-red">«</span> │        <span class="dark-grey">│</span> <span class="dark-magenta">+</span><span class="magenta">40</span> <span class="dark-magenta">to base stat:</span> <span class="magenta">100</span> <span class="dark-magenta">CP</span> <span class="dark-grey">│</span></div>
  <div> │ <span class="dark-red">»</span> <span class="dark-cyan">Health</span>     (#{String.rjust("#{hea}", 4)} to <span id="max-health">#{String.rjust("#{max_hea}", 4)}</span>)   <input id="health" class="field" maxlength="3" size="3" value="#{hea}"></input> <span class="dark-red">«</span> │        <span class="dark-grey">│</span> <span class="dark-magenta">+</span><span class="magenta">50</span> <span class="dark-magenta">to base stat:</span> <span class="magenta">150</span> <span class="dark-magenta">CP</span> <span class="dark-grey">│</span></div>
  <div> │ <span class="dark-red">»</span> <span class="dark-cyan">Charm</span>      (#{String.rjust("#{cha}", 4)} to <span id="max-charm">#{String.rjust("#{max_cha}", 4)}</span>)   <input id="charm" class="field" maxlength="3" size="3" value="#{cha}"></input> <span class="dark-red">«</span> │        <span class="dark-grey">└─    ... and so on ...   ─┘</span></div>
  <div> │                                     │</div>
  <div> │ <span class="dark-red">»</span>  <span class="dark-cyan">Gender</span>        <input id="gender" class="field" maxlength="15" size="15"></input>  <span class="dark-red">«</span> │        <span class="dark-grey">┌</span> <span class="cyan">Use the Space Bar to</span></div>
  <div> │ <span class="dark-red">»</span>  <span class="dark-cyan">Hair Length</span>   <input id="hair_length" class="field" maxlength="15" size="15"></input>  <span class="dark-red">«</span> │ <span class="arrow"><span class="dark-grey">◀──────┤</span> <span class="cyan">toggle between choices for</span></span></div>
  <div> │ <span class="dark-red">»</span>  <span class="dark-cyan">Hair Colour</span>   <input id="hair_color" class="field" maxlength="15" size="15"></input>  <span class="dark-red">«</span> │        <span class="dark-grey">└</span> <span class="cyan">your physical description</span></div>
  <div> │ <span class="dark-red">»</span>  <span class="dark-cyan">Eye Colour</span>    <input id="eye_color" class="field" maxlength="15" size="15"></input>  <span class="dark-red">«</span> │</div>
  <div> │                                     │</div>
  <div> │ <span class="dark-red">»</span>  <span class="dark-cyan">Exit:</span> <input type="button" id="save" class="field" value="SAVE"></input> <span class="red">«</span>  <span class="dark-red">»</span> <span class="dark-cyan">CP Left:</span>  <span id="cp">#{String.rjust("#{cp}", 3)}</span>  <span class="dark-red">«</span> │ <span class="arrow"><span class="dark-grey">◀───────</span> <span class="white">SAVE</span> <span class="cyan">your character or</span> <span class="white">EXIT</span></span></div>
  <div>┌┴───────────────────────────────.     │</div>
  <div>\\_________________________________\\___/</div>
  <div><span id="validation" class="red"></span></div>
  """
  ])
      Players.send_message(player, ["focus", "#first-name"])
    end

    def set_stat(player, stat_name, stat) do
      character = Components.Login.get_character(player)
      validate_stat(player, character, String.capitalize(stat_name), stat)
    end

    def validate_stat(player, character, stat_name, stat) do
      valid  = true
      number = 0
      min = :"Elixir.Components.#{stat_name}".value(character)
      max = max_stat(player, character, stat_name)
      current = Components.Login.get_stat(player, binary_to_atom(String.downcase(stat_name)))
      if Regex.match?(%r/^\d+$/, stat) do
        {number, _} = Integer.parse(stat)
        if number < min do
          Players.send_message(player, ["update", "#validation", "#{stat_name} may not be lower than #{min}."])
          valid = false
        end
        if number > max && number > current do
          Players.send_message(player, ["update", "#validation", "You only have enough CP to train #{stat_name} to #{max}."])
          valid = false
        end
      else
        Players.send_message(player, ["update", "#validation", "#{stat_name} must be a number."])
        valid = false
      end
      if valid do
        racial_min = racial_min_stat(character, stat_name)
        cp_change  = calculate_cp_change(racial_min, current, number)
        current_cp = Components.Login.get_cp(player)
        new_cp     = current_cp - cp_change
        Components.Login.set_cp(player, new_cp)
        Components.Login.set_stat(player, binary_to_atom(String.downcase(stat_name)), number)
        Players.send_message(player, ["update", "#cp", String.rjust("#{new_cp}", 3)])
        update_max_values(player, character)
      else
        stat_name = String.downcase(stat_name)
        old_stat = Components.Login.get_stat(player, binary_to_atom(stat_name))
        Players.send_message(player, ["set field", "##{stat_name}", old_stat])
        Players.send_message(player, ["focus", "##{stat_name}"])
      end
    end

    def max_stat(player, character, stat) do
      min = racial_min_stat(character, stat)
      current = Components.Login.get_stat(player, binary_to_atom(String.downcase(stat)))
      cp  = Components.Login.get_cp(player)
      calculate_max(min, current, cp)
    end

    def calculate_max(min, amount, cp) do
      cost = cp_cost(min, amount + 1)
      if cost > cp do
        amount
      else
        calculate_max(min, amount + 1, cp - cost)
      end
    end

    def racial_min_stat(character, stat) do
      race = Components.Race.value(character)
      :"Elixir.Components.#{stat}".value(race)
    end

    def cp_cost(min, target) do
      Float.ceil((target - min) / 10.0)
    end

    def calculate_cp_change(min, current, new) do
      lowest = Enum.min([current, new])
      Enum.reduce(current..new, 0, fn(point, total) ->
        if point == lowest do
          total
        else
          if new > current do
            total + cp_cost(min, point)
          else
            total - cp_cost(min, point)
          end
        end
      end)
    end

    def update_max_values(player, character) do
      stats = ["Strength", "Agility", "Intellect", "Willpower", "Health", "Charm"]
      Enum.each(stats, fn(stat) ->
        max_value = max_stat(player, character, stat)
        Players.send_message(player, ["update", "#max-#{String.downcase(stat)}", String.rjust("#{max_value}", 4)])
      end)
    end

    def hair_lengths do
      ["short", "shoulder-length", "long", "waist-length", "ankle-length", "none"]
    end

    def hair_colors do
      ["silver", "red", "brown", "dark-brown", "blonde", "green", "blue", "black", "white"]
    end

    def eye_colors do
      ["yellow", "pale-blue", "sea-blue", "dark-blue", "grey-blue", "slate-grey",
       "bright-green", "forest-green", "pale-green", "chesnut-brown", "dark-brown",
       "hazel", "violet", "lavender", "golden", "black", "crimson"]
    end

    def genders do
      ["female", "male"]
    end

    def cycle_options(player, field) do
      case field do
        "hair_length" ->
          current_length = Components.Login.get_hair_length(player)
          if current_length do
            current_index = Enum.find_index(hair_lengths, fn(hair_length) ->
              hair_length == current_length
            end)
            new_length = Enum.at(hair_lengths, current_index + 1, Enum.first(hair_lengths))
          else
            new_length = Enum.first(hair_lengths)
          end
          Components.Login.set_hair_length(player, new_length)
          Players.send_message(player, ["set field", "#hair_length", new_length])
        "hair_color" ->
          current_color = Components.Login.get_hair_color(player)
          if current_color do
            current_index = Enum.find_index(hair_colors, fn(hair_color) ->
              hair_color == current_color
            end)
            new_color = Enum.at(hair_colors, current_index + 1, Enum.first(hair_colors))
          else
            new_color = Enum.first(hair_colors)
          end
          Components.Login.set_hair_color(player, new_color)
          Players.send_message(player, ["set field", "#hair_color", new_color])
        "eye_color" ->
          current_color = Components.Login.get_eye_color(player)
          if current_color do
            current_index = Enum.find_index(eye_colors, fn(eye_color) ->
              eye_color == current_color
            end)
            new_color = Enum.at(eye_colors, current_index + 1, Enum.first(eye_colors))
          else
            new_color = Enum.first(eye_colors)
          end
          Components.Login.set_eye_color(player, new_color)
          Players.send_message(player, ["set field", "#eye_color", new_color])
        "gender" ->
          current_gender = Components.Login.get_gender(player)
          if current_gender do
            current_index = Enum.find_index(genders, fn(gender) ->
              gender == current_gender
            end)
            new_gender = Enum.at(genders, current_index + 1, Enum.first(genders))
          else
            new_gender = Enum.first(genders)
          end
          Components.Login.set_gender(player, new_gender)
          Players.send_message(player, ["set field", "#gender", new_gender])
        _ ->
      end
    end

    def validate_attribute(player, attribute_name, attribute) do
      valid_values = apply(Systems.Training, :"#{attribute_name}s", [])

      if Enum.member?(valid_values, attribute) do
        apply(Components.Login, :"set_#{attribute_name}", [player, attribute])
      else
        cycle_options(player, attribute_name)
        Players.send_message(player, ["focus", "##{attribute_name}"])
      end
    end

    def validate_name(player, name) do
      valid = true
      if String.length(name) == 0 do
        valid = false
        Players.send_message(player, ["update", "#validation", "Name cannot be blank."])
      end
      if Regex.match?(%r/[^a-zA-Z]/, name) do
        valid = false
        Players.send_message(player, ["update", "#validation", "Name can only include letters."])
      end
      if valid do
        Components.Login.set_name(player, name)
      else
        Players.send_message(player, ["focus", "#first-name"])
      end
    end

    def validate_last_name(player, name) do
      valid = true
      if Regex.match?(%r/[^a-zA-Z ]/, name) do
        valid = false
        Players.send_message(player, ["update", "#validation", "Last name can only include letters and spaces."])
      end
      if valid do
        Components.Login.set_last_name(player, name)
      else
        Players.send_message(player, ["focus", "#last-name"])
      end
    end

    def finish(player) do
      character = Components.Login.get_character(player)

      room = :global.whereis_name(:"82325")
      ApathyDrive.Entity.add_component(character, Components.CurrentRoom, room)

      ApathyDrive.Entity.add_component(character, Components.Name, Components.Login.get_name(player))
      ApathyDrive.Entity.add_component(character, Components.LastName, Components.Login.get_last_name(player))
      Components.Agility.value(character, Components.Login.get_stat(player, :agility))
      Components.Charm.value(character, Components.Login.get_stat(player, :charm))
      Components.Health.value(character, Components.Login.get_stat(player, :health))
      Components.Intellect.value(character, Components.Login.get_stat(player, :intellect))
      Components.Strength.value(character, Components.Login.get_stat(player, :strength))
      Components.Willpower.value(character, Components.Login.get_stat(player, :willpower))
      Components.CP.value(character, Components.Login.get_cp(player))
      Components.Gender.value(character, Components.Login.get_gender(player))
      Components.EyeColor.value(character, Components.Login.get_eye_color(player))
      Components.HairColor.value(character, Components.Login.get_hair_color(player))
      Components.HairLength.value(character, Components.Login.get_hair_length(player))

      ApathyDrive.Entity.save!(character)

      Players.send_message(player, ["clear scroll"])
      Systems.Room.display_room(player, room)

    end

end
