defmodule Systems.Training do
  use Systems.Reload

  def train_stats(player, character) do
    race       = Components.Race.value(character)
    race_name  = Components.Name.get_name(race)

    Entity.notify(player, {:training, character})

    Players.send_message(player, ["clear scroll"])
    Players.send_message(player, ["scroll",
"""
<div>   .─────────────────────────────────────.──.</div>
<div>  /  <span class="dark-grey">Apathy</span> <span class="dark-red">Drive</span> <span class="dark-cyan">Character Creation</span>    /    \\</div>
<div> │                                     ├──.   │
<div> │ <span class="dark-red">»</span> <span class="dark-cyan">Given Name</span>   <input id="first-name" class="field" maxlength="18" size="18"></input> <span class="dark-red">«</span> │___\\_/</div>
<div> │ <span class="dark-red">»</span> <span class="dark-cyan">Family Name</span>  <input id="last-name" class="field" maxlength="18" size="18"></input> <span class="dark-red">«</span> │</div>
<div> │ <span class="dark-red">»</span> <span class="dark-cyan">Race</span>         #{String.ljust(race_name, 19)}<span class="dark-red">«</span> │</div>
<div> │                                     │</div>
<div> │ <span class="dark-red">»</span> <span class="dark-cyan">Gender</span>       <input id="gender" class="field" maxlength="18" size="18"></input> <span class="dark-red">«</span> │        <span class="dark-grey">┌</span> <span class="cyan">Use the Space Bar to</span></div>
<div> │ <span class="dark-red">»</span> <span class="dark-cyan">Hair Length</span>  <input id="hair_length" class="field" maxlength="18" size="18"></input> <span class="dark-red">«</span> │ <span class="arrow"><span class="dark-grey">◀──────┤</span> <span class="cyan">toggle between choices for</span></span></div>
<div> │ <span class="dark-red">»</span> <span class="dark-cyan">Hair Colour</span>  <input id="hair_color" class="field" maxlength="18" size="18"></input> <span class="dark-red">«</span> │        <span class="dark-grey">└</span> <span class="cyan">your physical description</span></div>
<div> │ <span class="dark-red">»</span> <span class="dark-cyan">Eye Colour</span>   <input id="eye_color" class="field" maxlength="18" size="18"></input> <span class="dark-red">«</span> │</div>
<div> │                                     │</div>
<div> │ <span class="dark-red">»</span>  <span class="dark-cyan">Exit:</span> <input type="button" id="save" class="field" value="SAVE"></input> <span class="dark-red">«</span>                     │</div>
<div>┌┴───────────────────────────────.     │</div>
<div>\\_________________________________\\___/</div>
<div><span id="validation" class="red"></span></div>
"""
])
    Players.send_message(player, ["focus", "#first-name"])
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
          new_length = Enum.at(hair_lengths, current_index + 1, List.first(hair_lengths))
        else
          new_length = List.first(hair_lengths)
        end
        Components.Login.set_hair_length(player, new_length)
        Players.send_message(player, ["set field", "#hair_length", new_length])
      "hair_color" ->
        current_color = Components.Login.get_hair_color(player)
        if current_color do
          current_index = Enum.find_index(hair_colors, fn(hair_color) ->
            hair_color == current_color
          end)
          new_color = Enum.at(hair_colors, current_index + 1, List.first(hair_colors))
        else
          new_color = List.first(hair_colors)
        end
        Components.Login.set_hair_color(player, new_color)
        Players.send_message(player, ["set field", "#hair_color", new_color])
      "eye_color" ->
        current_color = Components.Login.get_eye_color(player)
        if current_color do
          current_index = Enum.find_index(eye_colors, fn(eye_color) ->
            eye_color == current_color
          end)
          new_color = Enum.at(eye_colors, current_index + 1, List.first(eye_colors))
        else
          new_color = List.first(eye_colors)
        end
        Components.Login.set_eye_color(player, new_color)
        Players.send_message(player, ["set field", "#eye_color", new_color])
      "gender" ->
        current_gender = Components.Login.get_gender(player)
        if current_gender do
          current_index = Enum.find_index(genders, fn(gender) ->
            gender == current_gender
          end)
          new_gender = Enum.at(genders, current_index + 1, List.first(genders))
        else
          new_gender = List.first(genders)
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
    if Regex.match?(~r/[^a-zA-Z]/, name) do
      valid = false
      Players.send_message(player, ["update", "#validation", "Name can only include letters."])
    end
    if Characters.name_taken?(name) do
      valid = false
      Players.send_message(player, ["update", "#validation", "There's already a character with that name."])
    end
    if valid do
      Components.Login.set_name(player, name)
    else
      Players.send_message(player, ["focus", "#first-name"])
    end
  end

  def validate_last_name(player, name) do
    valid = true
    if Regex.match?(~r/[^a-zA-Z ]/, name) do
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
    start_room_id = Components.find_by(Components.StartRoom, true) |> Components.ID.value

    Entity.add_component(character, Components.CurrentRoom, start_room_id)
    Entity.add_component(character, Components.Types, ["character"])
    Entity.add_component(character, Components.Name, Components.Login.get_name(player))
    Entity.add_component(character, Components.LastName, Components.Login.get_last_name(player))
    Entity.add_component(character, Components.Items, [])
    Entity.add_component(character, Components.Limbs, character |> Components.Race.value |> Components.Limbs.value)

    Components.Gender.value(character, Components.Login.get_gender(player))
    Components.EyeColor.value(character, Components.Login.get_eye_color(player))
    Components.HairColor.value(character, Components.Login.get_hair_color(player))
    Components.HairLength.value(character, Components.Login.get_hair_length(player))
    Components.HP.value(character, Systems.HP.max_hp(character))

    Entities.save!(character)

    Components.Login.login(player, character)

  end

end
