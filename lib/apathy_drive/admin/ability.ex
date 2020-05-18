defmodule ApathyDrive.AbilityAdmin do
  def index(_) do
    [
      id: nil,
      name: nil,
      command: nil,
      kind: nil,
      description: nil
    ]
  end

  def form_fields(_) do
    [
      name: nil,
      targets: nil,
      kind: nil,
      mana: nil,
      command: nil,
      description: nil,
      user_message: nil,
      target_message: nil,
      spectator_message: nil,
      duration: nil,
      cooldown: nil,
      cast_time: nil,
      energy: nil,
      difficulty: nil,
      level: nil,
      letter: nil
    ]
  end
end

# status: %{choices: [{"Publish", "publish"}, {"Pending", "pending"}]},
# body: %{type: :textarea, rows: 4},
# views: %{permission: :read},
# settings: %{label: "Post Settings"}
