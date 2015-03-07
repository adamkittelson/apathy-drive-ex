defmodule Commands.Grant do
  use ApathyDrive.Command

  def keywords, do: ["grant"]

  def execute(%Spirit{} = spirit, _arguments) do
    spirit
    |> Spirit.send_scroll("<p>You need a body to do that.</p>")
  end

  def execute(spirit, monster, arguments) do

    total = Components.Experience.value(spirit)

    exp = Regex.run(~r/\d+$/, Enum.join(arguments))
          |> Systems.Match.extract_number
          |> String.to_integer

    execute(spirit, monster, total, exp)
  end

  def execute(spirit, monster, _total, nil) do
    Spirit.send_scroll(spirit, "<p>Grant how much of your experience to #{Components.Name.value(monster)}?</p>")
  end

  def execute(spirit, _monster, total, exp) when exp > total do
    Spirit.send_scroll(spirit, "<p>You don't have #{exp} experience, you only have #{total}!</p>")
  end

  def execute(spirit, monster, _total, exp) do

    old_devs = Systems.Trainer.total_power(monster)

    Components.Experience.add(spirit, -exp)
    Components.Experience.add(monster, exp)
    Entities.save!(spirit)
    Entities.save!(monster)

    new_devs = Systems.Trainer.total_power(monster)
    dev_gain = new_devs - old_devs

    Spirit.send_scroll(spirit, "<p>You grant #{Components.Name.value(monster)} #{exp} experience.</p>")

    if dev_gain > 0 do
      Spirit.send_scroll(spirit, "<p>You gain #{dev_gain} development points.</p>")
    end
  end

end
