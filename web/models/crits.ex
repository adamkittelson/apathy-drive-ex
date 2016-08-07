defmodule ApathyDrive.Crits do
  use ApathyDrive.Web, :model
  use Timex

  schema "crits" do
    field :crit_table, :string
    field :letter, :string
    field :abilities, ApathyDrive.JSONB, default: []

    timestamps
  end

  def for_table(table) do
    __MODULE__
    |> where(crit_table: ^table)
  end

  def abilities(damage, crit_tables) do
    table = Enum.random(crit_tables)
    letter = roll_for_letter(damage)

    IO.puts "letter: #{inspect letter}"

    if letter do
      count =
        __MODULE__
        |> where(crit_table: ^table, letter: ^letter)
        |> select([crit], count(crit.id))
        |> Repo.one

      __MODULE__
      |> offset(fragment("floor(random()*?) LIMIT 1", ^count))
      |> select([crit], crit.abilities)
      |> Repo.one
    else
      []
    end
  end

  def roll_for_letter(crit_chance) do
    case :rand.uniform(1_000_000) do
      roll when roll > crit_chance * 10_000 ->
        nil
      roll when roll > crit_chance * 5000 ->
        "A"
      roll when roll > crit_chance * 2500 ->
        "B"
      roll when roll > crit_chance * 1250 ->
        "C"
      roll when roll > crit_chance * 625 ->
        "D"
      _ ->
        "E"
    end
  end

  defmacro __using__(_) do
    quote do
      def import! do
        name
        |> ApathyDrive.Crits.for_table
        |> ApathyDrive.Repo.delete_all

        crits
        |> Enum.each(fn {letter, abilities} ->
             Enum.each abilities, fn crit ->

               crit_abilities = []

               ability = %{
                 "kind" => "attack",
                 "cast_message" => %{
                   "user" => crit.user_message,
                   "target" => crit.target_message,
                   "spectator" => crit.spectator_message
                 }
               }

               ability =
                 if crit[:effects][:damage] do
                   ability = Map.put_new(ability, "instant_effects", %{})
                   put_in(ability["instant_effects"]["damage"], %{"potency" => crit[:effects][:damage] * 100, "type" => damage_type})
                 else
                   ability
                 end

               ability =
                 if crit[:effects][:limb_loss] do
                   ability = Map.put_new(ability, "instant_effects", %{})
                   put_in(ability["instant_effects"]["limb_loss"], Enum.map(crit[:effects][:limb_loss], fn limb_loss ->
                     %{
                       "kind" => limb_loss[:kind],
                       "limb" => limb_loss[:limb]
                     }
                   end))
                 else
                   ability
                 end

               crit_abilities = [ability]

               crit_abilities =
                 if crit[:effects][:damage_over_time] do
                   ability = %{
                     "kind" => "curse",
                     "duration" => crit[:effects][:damage_over_time][:duration],
                     "duration_effects" => %{
                       "effect_message" => "<p><span class='dark-magenta'>You take damage from your wounds!</span></p>",
                       "damage" => %{
                         "potency" => crit[:effects][:damage_over_time][:damage] * 100,
                         "type" => damage_type
                       }
                     }
                   }
                   [ability | crit_abilities]
                 else
                   crit_abilities
                 end

               crit_abilities =
                 if crit[:effects][:stun] do
                   ability = %{
                     "kind" => "curse",
                     "duration" => crit[:effects][:stun],
                     "duration_effects" => %{
                       "confusion_message" => %{
                         "user" => "<p><span class='dark-cyan'>You are stunned and cannot move!</span></p>",
                         "spectator" => "<p><span class='dark-cyan'>{{target}} is stunned and cannot move!</span></p>" 
                       },
                       "expiration_message" => "<p><span class='yellow'>YOU CAN MOVE AGAIN!</span></p>",
                       "confused" => 100
                     }
                   }
                   [ability | crit_abilities]
                 else
                   crit_abilities
                 end

               crit_abilities =
                 if crit[:effects][:stat_mod] do
                   Enum.reduce(crit[:effects][:stat_mod], crit_abilities, fn stat_mod, updated_crits ->
                     stat =
                       case stat_mod[:stat] || stat_mod[:skill] do
                         "strength" ->
                           "strength"
                         "agility" ->
                           "agility"
                         "intellect" ->
                           "will"
                          "parry" ->
                            nil
                          "dodge" ->
                            "dodge"
                           derp ->
                            IO.inspect(stat_mod)
                            raise "no stat"
                       end

                     if stat do
                       ability = %{
                         "kind" => "curse",
                         "duration" => stat_mod[:duration],
                         "duration_effects" => %{
                           stat => stat_mod[:amount]
                         }
                       }
                       [ability | updated_crits]
                     else
                       updated_crits
                     end
                   end)
                 else
                   crit_abilities
                 end

               ApathyDrive.Repo.insert!(%ApathyDrive.Crits{crit_table: name, letter: letter, abilities: crit_abilities})
             end
           end)
      end
    end
  end

end