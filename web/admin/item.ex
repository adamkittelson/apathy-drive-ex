defmodule ApathyDrive.ExAdmin.Item do
  use ExAdmin.Register

  register_resource ApathyDrive.Item do

    form item do
      inputs do
        
        input item, :name
        input item, :description
        input item, :weight
        input item, :worn_on
        input item, :level
        input item, :grade

        content do
          ~s(
          <div id="item_abilities_input" class="form-group">
            <label class="col-sm-2 control-label" for="item_abilities">
              Properties<abbr class="required" title="required">*</abbr>
            </label>
            <div class="col-sm-10">
              <textarea id="item_abilities" name="item[abilities]" class="json">#{Poison.encode!(item.abilities)}</textarea>
            </div>
          </div>
          )
        end

        input item, :global_drop

        
      end
    end

  end
end
