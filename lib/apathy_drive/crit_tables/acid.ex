defmodule ApathyDrive.CritTables.Acid do

  def name do
    "acid"
  end

  def damage_type do
    "magical"
  end

  def crits do
    %{
      "A"=>[
        %{
          :user_message=>"{{target}}'s skin sizzles a little.",
          :target_message=>"You skin sizzles a little.",
          :spectator_message=>"{{target}}'s skin sizzles a little.",
          :effects => %{ :damage => 0.5 }
        },
        %{
          :user_message=>"{{target}}'s is burned by your acid!",
          :target_message=>"You are burned by {{user}}'s acid!",
          :spectator_message=>"{{target}} is burned by {{user}}'s acid.",
          :effects => %{ :damage => 1.0 }
        }
      ],
      "B"=>[
        %{
          :user_message=>"{{target}} is burned painfully!",
          :target_message=>"You are burned painfully!.",
          :spectator_message=>"{{target}} is burned painfully.",
          :effects => %{ :damage => 1.25 }
        },
        %{
          :user_message=>"Your acid burns a hole in {{target}}'s skin!",
          :target_message=>"{{user}}'s acid burns a hole in your skin!",
          :spectator_message=>"{{target}}'s acid burns a hole in {{target}}'s skin.",
          :effects => %{ :damage => 1.5 }
        }
      ],
      "C"=>[
        %{
          :user_message      => "{{target}} will never be {{target:handsome/pretty/the same}} again, given that you just melted {{target:his/her/their}} face off.",
          :target_message    => "You will never be {{target:handsome/pretty/the same}} again, given that {{user}} just melted your face off.",
          :spectator_message => "{{target}} will never be {{target:handsome/pretty/the same}} again, given that {{user}} just melted {{target:his/her/their}} face off.",
          :effects => %{ :damage => 1.75 }
        },
        %{
          :user_message      => "{{target}}'s arm stops working, possibly because the muscle just melted off the bone.",
          :target_message    => "Your arm stops working, possibly because the muscle just melted off the bone.",
          :spectator_message => "{{target}}'s arm stops working, possibly because the muscle just melted off the bone.",
          :effects => %{ 
            :limb_loss => [%{ :kind => "cripple", :limb => "arm" }],
            :damage => 2.0 
          }
        }
      ],
      "D"=>[
        %{
          :user_message      => "Acid covers {{target}}'s body!",
          :target_message    => "Acid covers your body!",
          :spectator_message => "Acid covers {{target}}'s body!",
          :effects => %{
            :damage_over_time => %{ :damage => 1.0, :duration => 10.0}
          }
        }
      ],
      "E"=>[
        %{
          :user_message      => "Your acid melts {{target}} into a puddle on the floor.",
          :target_message    => "Acid melts you into a puddle on the floor.",
          :spectator_message => "Acid melts {{target}} into a puddle on the floor.",
          :effects => %{
            :damage_over_time => %{ :kill => true }
          }
        } 
      ]
    }
  end

end
