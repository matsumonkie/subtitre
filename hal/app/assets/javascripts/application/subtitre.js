$(document).on('turbolinks:load', function() {

  // select file modal
  $("#fake-upload-button").mouseup(function () {
    $('#file_input').click()
  })

  // check for form answer
  function resetForm() {
    var subtitreDownloaded = function() {
      return document.cookie.replace(/(?:(?:^|.*;\s*)subtitre_downloaded\s*\=\s*([^;]*).*$)|^.*$/, "$1");
    }
    function delete_cookie(name) {
      document.cookie = name +'=; Path=/; Expires=Thu, 01 Jan 1970 00:00:01 GMT;';
    }
    if (subtitreDownloaded() === "true") {
      $("#fake-upload-button").removeClass('is-loading');
      $("#file_input").val('');
      delete_cookie("subtitre_downloaded");
    }
  }
  setInterval(resetForm, 500);

  // submit form
  $("#file_input").change(function () {
    $("#fake-upload-button").addClass('is-loading');
    $("#submit").click();
  })
});
