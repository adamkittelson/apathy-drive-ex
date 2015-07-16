defmodule MonsterTemplateTest do
  use ApathyDrive.ModelCase
  use Timex

  test "monster template with nil regen time is not on cooldown" do
    mt = %MonsterTemplate{regen_time_in_minutes: nil, last_killed_at: Date.now}
    refute MonsterTemplate.on_cooldown?(mt)
  end

  test "monster template with nil last_killed_at is not on cooldown" do
    mt = %MonsterTemplate{regen_time_in_minutes: 5, last_killed_at: nil}
    refute MonsterTemplate.on_cooldown?(mt)
  end

  test "monster template with a 5 minute regen time that was killed 10 minutes ago is not on cooldown" do
    mt = %MonsterTemplate{regen_time_in_minutes: 5, last_killed_at: Date.now |> Date.shift(mins: -10)}
    refute MonsterTemplate.on_cooldown?(mt)
  end

  test "monster template with a 5 minute regen time that was killed 2 minutes ago is on cooldown" do
    mt = %MonsterTemplate{regen_time_in_minutes: 5, last_killed_at: Date.now |> Date.shift(mins: -2)}
    assert MonsterTemplate.on_cooldown?(mt)
  end
end
