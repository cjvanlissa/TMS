dt_wrap <- function(dat, childrow, options = NULL){
  colnum <- which(names(dat) == childrow)
  DT::datatable(
    escape = FALSE, # escape = -1,
    cbind(' ' = '<img src="https://datatables.net/examples/resources/details_open.png"/>', dat),
    options = c(list(
      columnDefs = list(
        list(visible = FALSE, targets = colnum),
        list(orderable = FALSE, className = 'details-control', targets = 0)
      )
    ), options),
    callback = DT::JS(paste0("table.column(0).nodes().to$().css({cursor: 'pointer'});
  var format = function(d) {
    return '<div style=\"background-color:#eee; padding: .5em;\"> ' +
            d[", colnum, "] + '</div>';
  };
  table.on('click', 'td.details-control', function() {
    var td = $(this), row = table.row(td.closest('tr'));
    if (row.child.isShown()) {
      row.child.hide();
      td.html('<img src=\"https://datatables.net/examples/resources/details_open.png\"/>');
    } else {
      row.child(format(row.data())).show();
      td.html('<img src=\"https://datatables.net/examples/resources/details_close.png\"/>');
    }
  });")
    ), rownames = FALSE)
}
