// Allow selecting checkboxes by clicking on the cell they are included in
$('td').click(function(event) {
  // Prevent clicking on a checkbox from triggering the cell click event again
  if (event.target.type !== 'checkbox') {
    var $cell = $(this);
    // Toggle each checkbox in the cell
    $cell.find('input[type="checkbox"]').each(function() {
      $(this).prop("checked", !$(this).prop("checked"));
    });
  }
});

