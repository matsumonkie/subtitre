$(document).on('turbolinks:load', function() {

  // form submission
  $("#file_input").change(function () {
    console.log("test")
    $(".loading-icon").removeClass('invisible');
    $("#submit").click();
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
      $(".loading-icon").addClass('invisible');
      $("#file_input").val('');
      delete_cookie("subtitre_downloaded");
    }
  }
  setInterval(resetForm, 500);

});
