defmodule ApathyDrive.ExAdmin.MonsterTemplate do
  use ExAdmin.Register

  register_resource ApathyDrive.MonsterTemplate do

    form monster_template do
      inputs do

        input monster_template, :name
        input monster_template, :description
        input monster_template, :death_message
        input monster_template, :enter_message
        input monster_template, :exit_message
        input monster_template, :greeting
        input monster_template, :gender, collection: %{"Male" => "male", "Female" => "female"}
        input monster_template, :game_limit
        content do
          ~s(
          <div id="monster_template_adjectives_input" class="form-group">
            <label class="col-sm-2 control-label" for="monster_template_adjectives">
              Adjectives<abbr class="required" title="required">*</abbr>
            </label>
            <div class="col-sm-10">
              <textarea id="monster_template_adjectives" name="monster_template[adjectives]" class="json">#{Poison.encode!(monster_template.adjectives)}</textarea>
            </div>
          </div>
          )
        end
        input monster_template, :chance_to_follow, type: :number
        input monster_template, :alignment
        input monster_template, :level
        content do
          ~s(
          <div id="monster_template_questions_input" class="form-group">
            <label class="col-sm-2 control-label" for="monster_template_questions">
              Questions<abbr class="required" title="required">*</abbr>
            </label>
            <div class="col-sm-10">
              <textarea id="monster_template_questions" name="monster_template[questions]" class="json">#{Poison.encode!(monster_template.questions)}</textarea>
            </div>
          </div>
          )
        end
        content do
          ~s(
          <div id="monster_template_flags" class="form-group">
            <label class="col-sm-2 control-label" for="monster_template_flags">
              Flags<abbr class="required" title="required">*</abbr>
            </label>
            <div class="col-sm-10">
              <textarea id="monster_template_flags" name="monster_template[flags]" class="json">#{Poison.encode!(monster_template.flags)}</textarea>
            </div>
          </div>
          )
        end
        input monster_template, :experience
        input monster_template, :permanent
        input monster_template, :movement
        content do
          ~s(
          <div id="monster_template_unities" class="form-group">
            <label class="col-sm-2 control-label" for="monster_template_unities">
              Unities<abbr class="required" title="required">*</abbr>
            </label>
            <div class="col-sm-10">
              <textarea id="monster_template_unities" name="monster_template[unities]" class="json">#{Poison.encode!(monster_template.unities)}</textarea>
            </div>
          </div>
          )
        end

      end
    end

  end
end
