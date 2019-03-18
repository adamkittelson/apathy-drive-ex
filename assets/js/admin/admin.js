import $ from "js/jquery-1.10.2.min";

$(document).ready(function () {
  $("#add_trait").click(function (event) {
    event.preventDefault()
    let time = new Date().getTime()
    let template = $(event.target).attr('data-template')
    var uniq_template = template.replace(/\[0]/g, `[${time}]`)
    uniq_template = uniq_template.replace(/\[0]/g, `_${time}_`)
    $("#traits").append(uniq_template)
  })
});
