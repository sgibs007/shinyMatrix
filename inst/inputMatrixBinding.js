var inputMatrixInputBinding = new Shiny.InputBinding();

$.extend(inputMatrixInputBinding, {
  find: function(scope) {
    return $(scope).find('.shiny-inputmatrix-container');
  },

  getValue: function(el) {
    var $rows = $(el).find("tr.shiny-matrix-row");
    var values = {};

    $rows.each(function() {
      var inputType = $(this).find("input").attr("type");
      var $inputs = $(this).find("input:" + inputType);
      var selectedValues = [];
      $inputs.each(function() {
        if ($(this).prop("checked")) {
          selectedValues.push($(this).val());
        }
      });

      // Convert array to a string with a suitable separator
      values[$(this).attr("name")] = selectedValues.length > 0 ? selectedValues.join("|") : null;
    });

    return values;
  },

  setValue: function(el, value) {
    var $rows = $(el).find("tr.shiny-matrix-row");
    $rows.find("input").prop("checked", false); // Reset all inputs
    $rows.each(function() {
      var rowName = $(this).attr("name");
      var rowValues = value[rowName];
      if (rowValues) {
        var valuesArray = rowValues.split("|");
        var inputType = $(this).find("input").attr("type");
        var $inputs = $(this).find("input:" + inputType);
        $inputs.each(function() {
          if (valuesArray.includes($(this).val())) {
            $(this).prop("checked", true);
          }
        });
      }
    });
  },
  subscribe: function(el, callback) {
    $(el).on('change.inputMatrixInputBinding', 'input', function(event) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off('.inputMatrixInputBinding');
  },
});

Shiny.inputBindings.register(inputMatrixInputBinding, 'shiny.inputMatrixInput');
