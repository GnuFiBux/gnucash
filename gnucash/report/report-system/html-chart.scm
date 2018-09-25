;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-chart.scm : generate HTML programmatically, with support
;; for simple style elements.
;;
;; Added dependency on guile-json to help construct options. Migrated
;; from obsolete jquery and jqplot to modern chartjs instead in 2018
;; by Christopher Lam
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (json builder))            ;for building JSON options

;; The html-chart specifies options and data to create a chart.  The
;; old chart toolkits guppi and jqplot seemed to require restrictive
;; specific formats for the options and data, and modern toolkit
;; chartjs is more permissive. Nonetheless some of the restrictions
;; remain.

;; At minimum the html-chart will require setting the following
;; fields:
;; type - one of 'bar 'line 'pie 'scatter
;; width - pair
;; height - pair
;; row-labels - X-axis values
;; col-labels - Y-axis values
;; colors - colors
;; data - a list of list of numbers

;; Note the data is currently a direct representation of a spreadsheet.
;; (list (list data1 data2 data3)
;;       (list data1 data2 data3)
;;       (list data1 data2 data3)
;;       (list data1 data2 data3)
;;       (list data1 data2 data3)
;;       (list data1 data2 data3)
;;       (list data1 data2 data3)
;;       (list data1 data2 data3))

(define <html-chart>
  (make-record-type "<html-chart>"
                    '(width
                      height
                      title
                      subtitle
                      x-axis-label
                      y-axis-label
                      col-labels
                      row-labels
                      colors
                      legend-reversed?
                      row-labels-rotated?
                      stacked?
                      type
                      markers
                      y-grid?
                      x-grid?
                      data
                      urls
                      line-width
                      currency-symbol)))

(define gnc:html-chart?
  (record-predicate <html-chart>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <html-chart> class
;;  generate the <object> form for an html chart.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gnc:make-html-chart-internal
  (record-constructor <html-chart>))

(define (gnc:make-html-chart)
  (gnc:make-html-chart-internal
   '(percent . 100)  ;;width
   '(percent . 100)  ;;height
   #f   ;;title
   #f   ;;subtitle
   #f   ;;x-axis-label
   #f   ;;y-axis-label
   '()  ;;col-labels
   '()  ;;row-labels
   '()  ;;col-colors
   #t   ;;legend-reversed?
   #f   ;;row-labels-rotated?
   #f   ;;stacked?
   'bar ;;type
   #t   ;;markers
   #t   ;;y-grid?
   #t   ;;x-grid?
   '()  ;;data
   #f   ;;urls
   1.5  ;;line-width
   "\u00A4"  ;;currency-symbol
   ))

(define gnc:html-chart-data
  (record-accessor <html-chart> 'data))

(define gnc:html-chart-set-data!
  (record-modifier <html-chart> 'data))

(define gnc:html-chart-width
  (record-accessor <html-chart> 'width))

(define gnc:html-chart-set-width!
  (record-modifier <html-chart> 'width))

(define gnc:html-chart-height
  (record-accessor <html-chart> 'height))

(define gnc:html-chart-set-height!
  (record-modifier <html-chart> 'height))

(define gnc:html-chart-x-axis-label
  (record-accessor <html-chart> 'x-axis-label))

(define gnc:html-chart-set-x-axis-label!
  (record-modifier <html-chart> 'x-axis-label))

(define gnc:html-chart-y-axis-label
  (record-accessor <html-chart> 'y-axis-label))

(define gnc:html-chart-set-y-axis-label!
  (record-modifier <html-chart> 'y-axis-label))

(define gnc:html-chart-row-labels
  (record-accessor <html-chart> 'row-labels))

(define gnc:html-chart-set-row-labels!
  (record-modifier <html-chart> 'row-labels))

(define gnc:html-chart-row-labels-rotated?
  (record-accessor <html-chart> 'row-labels-rotated?))

(define gnc:html-chart-set-row-labels-rotated?!
  (record-modifier <html-chart> 'row-labels-rotated?))

(define gnc:html-chart-stacked?
  (record-accessor <html-chart> 'stacked?))

(define gnc:html-chart-set-stacked?!
  (record-modifier <html-chart> 'stacked?))

(define gnc:html-chart-type
  (record-accessor <html-chart> 'type))

(define gnc:html-chart-set-type!
  (record-modifier <html-chart> 'type))

(define gnc:html-chart-markers
  (record-accessor <html-chart> 'markers))

(define gnc:html-chart-set-markers!
  (record-modifier <html-chart> 'markers))

(define gnc:html-chart-y-grid?
  (record-accessor <html-chart> 'y-grid?))

(define gnc:html-chart-set-y-grid?!
  (record-modifier <html-chart> 'y-grid?))

(define gnc:html-chart-x-grid?
  (record-accessor <html-chart> 'x-grid?))

(define gnc:html-chart-set-x-grid?!
  (record-modifier <html-chart> 'x-grid?))

(define gnc:html-chart-col-labels
  (record-accessor <html-chart> 'col-labels))

(define gnc:html-chart-set-col-labels!
  (record-modifier <html-chart> 'col-labels))

(define gnc:html-chart-colors
  (record-accessor <html-chart> 'colors))

(define gnc:html-chart-set-colors!
  (record-modifier <html-chart> 'colors))

(define gnc:html-chart-legend-reversed?
  (record-accessor <html-chart> 'legend-reversed?))

(define gnc:html-chart-set-legend-reversed?!
  (record-modifier <html-chart> 'legend-reversed?))

(define gnc:html-chart-title
  (record-accessor <html-chart> 'title))

(define gnc:html-chart-set-title!
  (record-modifier <html-chart> 'title))

(define gnc:html-chart-subtitle
  (record-accessor <html-chart> 'subtitle))

(define gnc:html-chart-set-subtitle!
  (record-modifier <html-chart> 'subtitle))

(define gnc:html-chart-urls
  (record-accessor <html-chart> 'urls))

(define gnc:html-chart-set-urls!
  (record-modifier <html-chart> 'urls))

(define gnc:html-chart-line-width
  (record-accessor <html-chart> 'line-width))

(define gnc:html-chart-set-line-width!
  (record-modifier <html-chart> 'line-width))

(define gnc:html-chart-currency-symbol
  (record-accessor <html-chart> 'currency-symbol))

(define gnc:html-chart-set-currency-symbol!
  (record-modifier <html-chart> 'currency-symbol))

(define (JS-setup id)
  (format #f "// draw the background color
Chart.pluginService.register({
  beforeDraw: function (chart, easing) {
    if (chart.config.options.chartArea && chart.config.options.chartArea.backgroundColor) {
      var ctx = chart.chart.ctx;
      var chartArea = chart.chartArea;
      ctx.save();
      ctx.fillStyle = chart.config.options.chartArea.backgroundColor;
      ctx.fillRect(chartArea.left, chartArea.top, chartArea.right - chartArea.left, chartArea.bottom - chartArea.top);
      ctx.restore();
    }
  }
})

document.getElementById(chartid).onclick = function(evt) {
  var activepoints = myChart.getElementAtEvent(evt);
  var anchor = document.getElementById('jump-~a');
  if (activepoints.length > 0) {
    var index = activepoints[0]['_index'];
    var datasetIndex = activepoints[0]['_datasetIndex'];
    if (charttype === 'pie') {
      var url = url1[index];
    } else {
      var url = url1[datasetIndex];
    }
    anchor.href = url;
    // anchor.style = 'position:absolute; top:' + (evt.clientY - 30) + 'px; left:' + evt.clientX + 'px';
    anchor.textContent = 'Load ' + url.substring(4, url.indexOf(':'));
  } else {
    anchor.href = '';
    anchor.textContent = '';
  }
}\n\n" id))

(define (gnc:html-chart-append-row! chart newrow)
  (let ((dd (gnc:html-chart-data chart)))
    (set! dd (append dd (list newrow)))
    (gnc:html-chart-set-data! chart dd)))

(define (gnc:html-chart-prepend-row! chart newrow)
  (let ((dd (gnc:html-chart-data chart)))
    (set! dd (cons newrow dd))
    (gnc:html-chart-set-data! chart dd)))

(define (gnc:html-chart-append-column! chart newcol)
  (let* ((rownum 0)
         (rows (gnc:html-chart-data chart))
         (colnum (apply max (cons 0 (map length rows))))
         (this-row #f)
         (new-row #f))

    ;; append the elements of 'newrow' to the rowumns
    (for-each
     (lambda (newelt)
       ;; find the row, or append one
       (if (null? rows)
           (begin
             (set! new-row #t)
             (set! this-row '()))
           (begin
             (set! new-row #f)
             (set! this-row (car rows))
             (if (null? (cdr rows))
                 (set! rows #f)
                 (set! rows (cdr rows)))))

       ;; make sure the rowumn is long enough, then append the data
       (let loop ((l (length this-row))
                  (r (reverse this-row)))
         (if (< l colnum)
             (loop (1+ l) (cons #f r))
             (set! this-row
               (reverse (cons newelt r)))))
       (if new-row
           (gnc:html-chart-append-row! chart this-row)
           (list-set! (gnc:html-chart-data chart) rownum this-row))
       (set! rownum (1+ rownum)))
     newcol)))

(define (gnc:not-all-zeros data)
  (define (myor lst)
    (and (not (null? lst))
         (or (car lst) (myor (cdr lst)))))

  (cond ((number? data) (not (zero? data)))
        ((list? data) (myor (map gnc:not-all-zeros data)))
        (else #f)))

(define (gnc:html-chart-prepend-column! chart newcol)
  (let ((rows (gnc:html-chart-data chart))
        (this-row #f)
        (new-row #f)
        (rownum 0))
    (for-each
     (lambda (elt)
       (if (null? rows)
           (begin
             (set! new-row #t)
             (set! this-row '()))
           (begin
             (set! new-row #f)
             (set! this-row (car rows))
             (if (null? (cdr rows))
                 (set! rows #f)
                 (set! rows (cdr rows)))))
       (if new-row
           (gnc:html-chart-append-row! chart (list elt))
           (list-set! (gnc:html-chart-data chart) rownum
                      (cons elt this-row)))
       (set! rownum (1+ rownum)))
     newcol)))

(define (gnc:html-chart-render chart doc)

  (define (size->str size)
    (string-append (number->string (cdr size))
                   (case (car size)
                     ((pixels) "px")
                     ((percent) "%"))))

  (let* ((retval '())
         (push (lambda (l) (set! retval (cons l retval))))
         (charttype (gnc:html-chart-type chart))
         (currency (gnc:html-chart-currency-symbol chart))
         (data (gnc:html-chart-data chart))
         ;; Use a unique chart-id for each chart. This prevents charts
         ;; clashing on multi-column reports
         (id (random 99999999)))
    (if (and (list? data)
             (not (null? data))
             (gnc:not-all-zeros data))
        (begin

          (push (format #f "<script language='javascript' type='text/javascript' src='file://~a'></script>\n"
                        (gnc-path-find-localized-html-file "chartjs/Chart.bundle.min.js")))

          (push (format #f "<div style='width:~a;height:~a;'>\n"
                        (size->str (gnc:html-chart-width chart))
                        (size->str (gnc:html-chart-height chart))))
          (push (format #f "<a id='jump-~a' style='position:absolute; top:10px; left:10px' href=''></a>\n" id))
          (push (format #f "<canvas id='chart-~a'></canvas>\n" id))
          (push "</div>\n")
          (push (format #f "<script id='script-~a'>\n" id))
          (push (format #f "var all_ticks = ~a;\n" (scm->json-string (gnc:html-chart-row-labels chart))))
          (push (format #f "var url1 = ~a;\n" (scm->json-string (gnc:html-chart-urls chart))))
          (push (format #f "var charttype = ~s;\n" (symbol->string charttype)))
          (push (format #f "var chartid = 'chart-~a';\n" id))

          (let ((chartjs-options
                 (list
                  (cons 'type (case charttype
                                ((scatter) 'line)
                                (else charttype)))
                  (cons 'options (list
                                  (cons 'maintainAspectRatio #f)
                                  (cons 'chartArea (list
                                                    (cons 'backgroundColor "#fffdf6")))
                                  (cons 'legend (list
                                                 (cons 'position 'right)
                                                 (cons 'reverse (gnc:html-chart-legend-reversed? chart))
                                                 (cons 'labels (list
                                                                (cons 'fontColor 'black)))))
                                  (cons 'elements (list
                                                   (cons 'line (list
                                                                (cons 'tension 0)))
                                                   (cons 'point (list
                                                                 (cons 'pointStyle (gnc:html-chart-markers chart))))))
                                  (cons 'tooltips (list
                                                   (cons 'callbacks (list
                                                                     (cons 'label #f)))))
                                  (cons 'scales (list
                                                 (cons 'xAxes (list
                                                               (list
                                                                (cons 'display (not (eq? charttype 'pie)))
                                                                (cons 'type (if (eq? charttype 'scatter)
                                                                                'linear
                                                                                'category))
                                                                (cons 'distribution (case charttype
                                                                                      ((scatter) 'linear)
                                                                                      (else 'series)))
                                                                (cons 'offset (and (memq charttype '(bar scatter)) #t))
                                                                (cons 'gridLines (list
                                                                                  (cons 'display (gnc:html-chart-x-grid? chart))
                                                                                  (cons 'lineWidth (gnc:html-chart-line-width chart))))
                                                                (cons 'scaleLabel (list
                                                                                   (cons 'display (and (gnc:html-chart-x-axis-label chart) #t))
                                                                                   (cons 'labelString (gnc:html-chart-x-axis-label chart))))
                                                                (cons 'ticks (list
                                                                              (cons 'fontSize 12)
                                                                              (cons 'maxRotation 30))))))
                                                 (cons 'yAxes (list
                                                               (list
                                                                (cons 'stacked (gnc:html-chart-stacked? chart))
                                                                (cons 'display (not (eq? charttype 'pie)))
                                                                (cons 'gridLines (list
                                                                                  (cons 'display (gnc:html-chart-y-grid? chart))
                                                                                  (cons 'lineWidth (gnc:html-chart-line-width chart))))
                                                                (cons 'scaleLabel (list
                                                                                   (cons 'display (and (gnc:html-chart-y-axis-label chart) #t))
                                                                                   (cons 'labelString (gnc:html-chart-y-axis-label chart))))
                                                                (cons 'ticks (list
                                                                              (cons 'fontSize 10)
                                                                              (cons 'beginAtZero #f))))))))
                                  (cons 'title (let ((t1 (gnc:html-chart-title chart))
                                                     (t2 (gnc:html-chart-subtitle chart)))
                                                 (list
                                                  (cons 'display (and (or t1 t2) #t))
                                                  (cons 'fontSize 16)
                                                  (cons 'fontStyle "")
                                                  (cons 'text (if t1 (if t2 (list t1 t2) t1) t2)))))))
                  (case charttype
                    ((pie)
                     (cons 'data (list
                                  (cons 'labels (gnc:html-chart-row-labels chart))
                                  (cons 'datasets (list
                                                   (list (cons 'data data)
                                                         (cons 'backgroundColor (gnc:html-chart-colors chart))))))))
                    ((scatter)
                     (cons 'data (list
                                  (cons 'datasets (list
                                                   (list (cons 'data
                                                               (map
                                                                (lambda (points)
                                                                  (list (cons 'x (car points))
                                                                        (cons 'y (cadr points))))
                                                                data))
                                                         (cons 'fill #f)
                                                         (cons 'backgroundColor (gnc:html-chart-colors chart))
                                                         (cons 'pointBorderColor (gnc:html-chart-colors chart))
                                                         (cons 'pointBorderWidth 3)
                                                         (cons 'borderColor "rgb(84,182,182)")))))))
                    (else
                     (cons 'data (list
                                  (cons 'labels (if (eq? charttype 'bar)
                                                    (map 1+ (iota (length (gnc:html-chart-row-labels chart))))
                                                    (gnc:html-chart-row-labels chart)))
                                  (cons 'datasets (map (lambda (rownum col collabel color)
                                                         (list (cons 'label collabel)
                                                               (cons 'data col)
                                                               (cons 'backgroundColor color)
                                                               (cons 'stack (if (gnc:html-chart-stacked? chart)
                                                                                'default ;if stacking, we'll limit ourself to 1 stack
                                                                                (number->string rownum))) ;not stackng, unique string
                                                               (cons 'fill #f)
                                                               (cons 'showLine #t)
                                                               (cons 'borderColor color)
                                                               (cons 'lineTension 0)))
                                                       (iota (length (gnc:html-chart-row-labels chart)))
                                                       (apply zip data)
                                                       (gnc:html-chart-col-labels chart)
                                                       (gnc:html-chart-colors chart))))))))))
            (push (format #f "var chartjsoptions = ~a;\n\n" (scm->json-string chartjs-options #:pretty #t))))
          (push "chartjsoptions.options.scales.yAxes[0].ticks.callback = yAxisDisplay;\n")
          (push "chartjsoptions.options.scales.xAxes[0].ticks.callback = xAxisDisplay;\n")
          (push "chartjsoptions.options.tooltips.callbacks.label = tooltipLabel;\n")
          (push "chartjsoptions.options.tooltips.callbacks.title = tooltipTitle;\n")

          (push "Chart.defaults.global.defaultFontFamily = \"'Trebuchet MS', Arial, Helvetica, sans-serif\";\n")
          (push (format #f "function yAxisDisplay(value,index,values) { return ~s + value.toLocaleString(); };\n" currency))
          (push "function xAxisDisplay(value,index,values) { return all_ticks[index]; };\n\n")

          ;; custom tooltip label: display series label, and value as monetary
          (push (format #f "function tooltipLabel(tooltipItem,data) {
                    var datasetLabel = data.datasets[tooltipItem.datasetIndex].label || 'Other';
                    var label = data.datasets[tooltipItem.datasetIndex].data[tooltipItem.index];
                    return datasetLabel + ': ' + ~s + label.toLocaleString(); }\n\n" currency))

          ;; custom tooltip title: retrieve label from all_ticks
          (push "function tooltipTitle(array,data) {
                    return all_ticks[array[0].index]; }\n\n")

          (push (JS-setup id))

          (push "var myChart = new Chart(chartid, chartjsoptions);\n")
          (push "</script>"))
        (gnc:warn "chart has no non-zero data."))
    retval))
