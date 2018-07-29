defmodule ApathyDriveWeb.Admin.ClassView do
  use ApathyDriveWeb, :view
  alias ApathyDrive.{Class, ClassTrait}

  def link_to_trait_fields do
    changeset = Class.changeset(%Class{classes_traits: [%ClassTrait{}]})
    form = Phoenix.HTML.FormData.to_form(changeset, [])
    fields = render_to_string(__MODULE__, "trait_fields.html", f: form)

    link(
      "Add Trait",
      to: "#",
      "data-template": fields,
      id: "add_trait",
      class: "button button-primary"
    )
  end
end
