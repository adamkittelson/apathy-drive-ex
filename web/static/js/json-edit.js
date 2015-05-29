$('textarea.json').each(function() {
  var textarea = $(this);

  var myCodeMirror = CodeMirror.fromTextArea(textarea.get(0), {
    mode:  {name: "javascript", json: true},
    lineNumbers: true,
    smartIndent: true,
    theme: "twilight",
    matchBrackets: true
  });

  myCodeMirror.setValue(JSON.stringify(JSON.parse(myCodeMirror.getValue()), null, '  '));
});

$(document).on('keyup', "#search", function(event) {
  if (event.which === 13) {
    var query = $('#search').val();
    window.location = "" + window.location.origin + window.location.pathname + "?q=" + query;
  }
});
