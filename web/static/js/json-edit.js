import {formatJson} from "web/static/js/json-format";

$(".json").linedtextarea()

$('textarea.json').each(function(index, textarea) {

  reformat(textarea);

});

$('textarea.json').on('change', function() {
  validate($(this).context);
});

function reformat(field) {
  field.value = formatJson(field.value);
}

function validate(textarea) {
  try {
    var result = parser.parse(textarea.value);
    if (result) {
      reformat(textarea);
    }
  } catch(e) {
    alert("Invalid JSON in: " + textarea.name + ":\n\n" + e);
  }
}

$(document).on('keyup', "#search", function(event) {
  if (event.which === 13) {
    var query = $('#search').val();
    window.location = "" + window.location.origin + window.location.pathname + "?q=" + query;
  }
});
