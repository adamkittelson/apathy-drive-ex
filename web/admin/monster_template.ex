defmodule ApathyDrive.ExAdmin.MonsterTemplate do
  use ExAdmin.Register

  register_resource ApathyDrive.MonsterTemplate do

    form monstertemplate do
      inputs do

        input monstertemplate, :name
        input monstertemplate, :description
        input monstertemplate, :death_message
        input monstertemplate, :enter_message
        input monstertemplate, :exit_message
        input monstertemplate, :greeting
        input monstertemplate, :gender, collection: %{"male" => "Male", "female" => "Female"}
        input monstertemplate, :game_limit
        content do
          ~s(
          <div id="monstertemplate_adjectives_input" class="form-group">
            <label class="col-sm-2 control-label" for="monstertemplate_adjectives">
              Adjectives<abbr class="required" title="required">*</abbr>
            </label>
            <div class="col-sm-10">
              <textarea id="monstertemplate_adjectives" name="monstertemplate[adjectives]" class="json">#{Poison.encode!(monstertemplate.adjectives)}</textarea>
            </div>
          </div>
          )
        end
        input monstertemplate, :chance_to_follow, type: :number
        input monstertemplate, :alignment
        input monstertemplate, :level
        content do
          ~s(
          <div id="monstertemplate_questions_input" class="form-group">
            <label class="col-sm-2 control-label" for="monstertemplate_questions">
              Questions<abbr class="required" title="required">*</abbr>
            </label>
            <div class="col-sm-10">
              <textarea id="monstertemplate_questions" name="monstertemplate[questions]" class="json">#{Poison.encode!(monstertemplate.questions)}</textarea>
            </div>
          </div>
          )
        end
        content do
          ~s(
          <div id="monstertemplate_flags" class="form-group">
            <label class="col-sm-2 control-label" for="monstertemplate_flags">
              Flags<abbr class="required" title="required">*</abbr>
            </label>
            <div class="col-sm-10">
              <textarea id="monstertemplate_flags" name="monstertemplate[flags]" class="json">#{Poison.encode!(monstertemplate.flags)}</textarea>
            </div>
          </div>
          )
        end
        input monstertemplate, :experience
        input monstertemplate, :permanent
        input monstertemplate, :movement, collection: %{"solo" => "Solo", "leader" => "Leader", "follower" => "Follower", "stationary" => "Stationary"}
        content do
          ~s(
          <div id="monstertemplate_unities" class="form-group">
            <label class="col-sm-2 control-label" for="monstertemplate_unities">
              Unities<abbr class="required" title="required">*</abbr>
            </label>
            <div class="col-sm-10">
              <textarea id="monstertemplate_unities" name="monstertemplate[unities]" class="json">#{Poison.encode!(monstertemplate.unities)}</textarea>
            </div>
          </div>
          )
        end

      end
    end

  end
end
