defmodule ApathyDrive.ExAdmin.Class do
  use ExAdmin.Register

  register_resource ApathyDrive.Class do
    
    index do
      selectable_column

      column :id
      column :name
      column :armour
      column :weapon
      actions
    end

    form class do
      inputs do
        input class, :name
        content do
          ~s(
          <div id="class_description_input" class="form-group">
            <label class="col-sm-2 control-label" for="class_description">
              Description<abbr class="required" title="required">*</abbr>
            </label>
            <div class="col-sm-10">
              <textarea id="class_description" name="class[description]">#{class.description}</textarea>
            </div>
          </div>
          )
        end
        input class, :armour, collection: ["Cloth", "Leather", "Chain", "Scale", "Plate"]
        input class, :weapon, collection: ["Any", "Blunt", "Bladed", "One Handed", "One Handed Blunt", "One Handed Bladed", "Limited"]

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
      end
    end

  end
end
