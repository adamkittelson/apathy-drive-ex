defmodule ApathyDrive.ExAdmin.Class do
  use ExAdmin.Register

  register_resource ApathyDrive.Class do

    form class do
      inputs do
        input class, :name
        input class, :alignment
        input class, :start_room_id

        content do
          ~s(
          <div id="class_abilities_input" class="form-group">
            <label class="col-sm-2 control-label" for="class_abilities">
              Abilities<abbr class="required" title="required">*</abbr>
            </label>
            <div class="col-sm-10">
              <textarea id="class_abilities" name="class[abilities]" class="json">#{Poison.encode!(class.abilities)}</textarea>
            </div>
          </div>
          )
        end

        content do
          ~s(
          <div id="class_unities_input" class="form-group">
            <label class="col-sm-2 control-label" for="class_unities">
              Properties<abbr class="required" title="required">*</abbr>
            </label>
            <div class="col-sm-10">
              <textarea id="class_unities" name="class[unities]" class="json">#{Poison.encode!(class.unities)}</textarea>
            </div>
          </div>
          )
        end
      end
    end

  end
end
