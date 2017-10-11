$(document).on('turbolinks:load', function() {

  // form submission
  $("#file_input").change(function () {
    $(".loading-icon").removeClass('invisible');
    $("#submit").click();
  })


  // level explanation
  function changeLevelText () {
    var level = $('input[name="subtitle[level]"]:checked').val();
    if (level == "easy") {
      var levelHelp = "Most words will be translated"
    } else if (level == "normal") {
      var levelHelp = "Easy words won't be translated"
    } else {
      var levelHelp = "Only words considered to be hard will be translated"
    }

    $("#level-explanation").text(levelHelp);
  }
  $('input[name="subtitle[level]"]').change(function() {
    changeLevelText();
  })
  changeLevelText();

  // form language constraints
  var selectFrom = "#subtitle_translate_from";
  var selectTo = "#subtitle_translate_to";

  $(selectFrom).change(function() {
    adaptDestinationLanguages();
  });
  adaptDestinationLanguages();

  function adaptDestinationLanguages () {
    function createOption(lang, humanLang) {
      return $('<option></option>').attr("value", lang).text(humanLang);
    }

    if ($(selectFrom).val() == "en") {
      var allLang = {
        "ar": "Arabic",
        "cz": "Czech",
        "en": "English",
        "es": "Spanish",
        "fr": "French",
        "gr": "Greek",
        "it": "Italian",
        "ja": "Japanese",
        "ko": "Korean",
        "po": "Polish",
        "pt": "Portuguese",
        "ro": "Romanian",
        "tr": "Turkish",
        "zh": "Chinese",
      };
      $(selectTo).empty();
      $.each(allLang, function(key, value) {
        $(selectTo).append(createOption(key, value));
      });
    } else {
      $(selectTo).empty().append(createOption("en", "English"));
    }
  }

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
      $('#close-modal-btn').click();
      delete_cookie("subtitre_downloaded");
    }
  }
  setInterval(resetForm, 500);

});
