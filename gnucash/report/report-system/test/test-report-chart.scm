(use-modules (gnucash gnc-module))

(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))

(use-modules (gnucash core-utils))
	
(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report report-system))
(use-modules (srfi srfi-64))
(use-modules (gnucash engine test srfi64-extras))

;; Explicitly set locale to make the report output predictable
(setlocale LC_ALL "C")

(define (run-test)
    (test-runner-factory gnc:test-runner)
    (test-begin "Testing/Temporary/test-report-chart") ;; if (test-runner-factory gnc:test-runner) is commented out, this
                                                       ;; will create Testing/Temporary/test-report-chart.log
    ;; (test-availability-chartjs)
    ;; in order to run this test successfully
    ;; set GNC_DOC_PATH and make it point to $PREFIX/share/gnucash
    ;; e.g.:
    ;; GNC_DOC_PATH="/usr/local/share/gnucash/"
    ;; export GNC_DOC_PATH

    ;; the following tests are independent of GNC_DOC_PATH
    (test-chart-zero-detection)
    (test-chart-data)
    (test-generic-chart-rendering)
    (test-bar-chart-rendering)
    (test-pie-chart-rendering)
    (test-scatter-chart-rendering)
    (test-end "Testing/Temporary/test-report-chart")
)

   ;; html bar charts and line charts
   ;;
   ;; The bar chart and line chart data is modelled in a table matrix.
   ;; The rows give the dates for the data.
   ;; The colums give the accounts from which the data is fetched.
   ;;
   ;; Example:
   ;;               Acct 1    Acct 2    Acct 3
   ;; 1990-01-01      1.00      4.00      7.00
   ;; 1990-02-01      2.00      5.00      8.00
   ;; 1990-03-01      3.00      6.00      9.00
   ;;
   ;; The data is coded is a list of rows, each row is a list
   ;; of columns and stored in the data field of the
   ;; html-chart/linechart record.
   ;;
   ;; The following list keeps the date from the example above:
   ;;
   ;; (
   ;;   (1.00 4.00 7.00)
   ;;   (2.00 5.00 8.00)
   ;;   (3.00 6.00 9.00)
   ;; )
   ;;
   ;; The row labels (here: 1990-01-01 ... 1990-03-01) and the
   ;; column labels (here: Acct 1 ... Acct 3) are stored as separate
   ;; lists in the fields row-labels and col-labels.


;; -----------------------------------------------------------------------

(define (test-availability-chartjs)

  (test-begin "HTML Chart Engine detection")

  (let (
         (retval (gnc-path-find-localized-html-file "chartjs/Chart.bundle.min.js"))
       )
    (test-assert "Chart Engine Detected" (not (string-null? retval))))

  (test-end "HTML Chart Engine detection")
)

;; -----------------------------------------------------------------------

(define (test-chart-zero-detection)

  (test-begin "HTML Chart zero-data detection")

  (let (
         (chart (gnc:make-html-chart))
       )
    ;; no data, no rendering - check that zero values are detected
    (gnc:html-chart-set-data! chart '())
    (test-assert "HTML Chart - zero detection - empty list" (not (gnc:not-all-zeros (gnc:html-chart-data chart))))
    (gnc:html-chart-set-data! chart '(() () ()))
    (test-assert "HTML Chart - zero detection - list of empty lists" (not (gnc:not-all-zeros (gnc:html-chart-data chart))))
    (gnc:html-chart-set-data! chart 0)
    (test-assert "HTML Chart - zero detection - integer 0" (not (gnc:not-all-zeros (gnc:html-chart-data chart))))
    (gnc:html-chart-set-data! chart 0.00)
    (test-assert "HTML Chart - zero detection - float 0" (not (gnc:not-all-zeros (gnc:html-chart-data chart))))
    (gnc:html-chart-set-data! chart '((0) (0) (0)))
    (test-assert "HTML Chart - zero detection - list of integer 0" (not (gnc:not-all-zeros (gnc:html-chart-data chart))))
    (gnc:html-chart-set-data! chart '((0.00) (0.00) (0.00)))
    (test-assert "HTML Chart - zero detection - list of float 0" (not (gnc:not-all-zeros (gnc:html-chart-data chart))))

    ;; check that non-zero values are detected
    (gnc:html-chart-set-data! chart 2)
    (test-assert "HTML Chart - non-zero detection - integer" (gnc:not-all-zeros (gnc:html-chart-data chart)))
    (gnc:html-chart-set-data! chart 2.00)
    (test-assert "HTML Chart - non-zero detection - float" (gnc:not-all-zeros (gnc:html-chart-data chart)))
    (gnc:html-chart-set-data! chart '((0) (2) (0)))
    (test-assert "HTML Chart - non-zero detection - list of integers" (gnc:not-all-zeros (gnc:html-chart-data chart)))
    (gnc:html-chart-set-data! chart '((0.00) (0.00) (2.00)))
    (test-assert "HTML Chart - non-zero detection - list of floats" (gnc:not-all-zeros (gnc:html-chart-data chart)))
    (gnc:html-chart-set-data! chart '(() () (3)))
    (test-assert "HTML Chart - non-zero detection - mixes element types" (gnc:not-all-zeros (gnc:html-chart-data chart)))
  )

  (test-end "HTML Chart zero-data detection")
)
;; -----------------------------------------------------------------------

(define (test-chart-data)

  (test-begin "HTML Chart Data")

  (let (
         (chart (gnc:make-html-chart))
       )
    (gnc:html-chart-append-row! chart '(1.0 2.0 3.0))
    (gnc:html-chart-append-row! chart '(4.00 5.00 6.00))
    (test-equal "HTML Chart Data - check append row"
      '((4.0 5.0 6.0) (1.0 2.0 3.0))
      (gnc:html-chart-data chart))
  )

  (let (
         (chart (gnc:make-html-chart))
       )
    (gnc:html-chart-set-data! chart '((1.0 2.0 3.0)))
    (gnc:html-chart-prepend-row! chart '(4.00 5.00 6.00))
    (test-equal "HTML Chart Data - check prepend row"
      '((1.0 2.0 3.0) (4.0 5.0 6.0))
      (gnc:html-chart-data chart))
  )

  (let (
         (chart (gnc:make-html-chart))
       )
    (gnc:html-chart-set-data! chart '((1.0 2.0 3.0)))
    (gnc:html-chart-append-column! chart '(7.00 8.00 9.00))
    (test-equal "HTML Chart Data - check append column"
      '((9.0) (8.0) (1.0 2.0 3.0 7.0))
      (gnc:html-chart-data chart))
  )

  (let (
         (chart (gnc:make-html-chart))
       )
    (gnc:html-chart-set-data! chart '((1.0 2.0)))
    (gnc:html-chart-append-row! chart '(3.00 4.00))
    (gnc:html-chart-prepend-column! chart '(5.00 6.00 7.00))
    (test-equal "HTML Chart Data - check prepend column"
      '((7.0) (6.0 3.0 4.0) (5.0 1.0 2.0))
      (gnc:html-chart-data chart))
  )

  (test-end "HTML Chart Data")
)

;; -----------------------------------------------------------------------

(define (test-generic-chart-rendering)

  (test-begin "HTML Generic Chart Rendering")

  (let* (
         (chart (gnc:make-html-chart))
         (retval ((lambda (c) 
                    (gnc:html-chart-set-type! c 'bar)
                    (gnc:html-chart-set-row-labels! c '("date1" "date2" "date3"))
                    (gnc:html-chart-set-col-labels! c '("dataset1" "dataset2" "dataset3"))
                    (gnc:html-chart-set-data! chart '((25 45 30)))
                    (gnc:html-chart-append-row! c '(65 50 40))
                    (gnc:html-chart-append-row! c '(75 55 70))
                    (gnc:html-chart-set-colors! chart '("red" "green" "blue"))
                    (reverse (gnc:html-chart-render c #f)))
                  chart))
       )


    (test-assert "Renderer returns a list" (list? retval))

    (test-equal "Renderer returns expected amount of values"
      23
      (length retval))

    (test-equal "Renderer returns expected Script Header"
      "<script language='javascript' type='text/javascript' src='file://"
      (string-take (list-ref retval 0) 65))

    (test-equal "Renderer returns expected style setting width/height"
      "<div style='width:100%;height:100%;'>\n"
      (list-ref retval 1))

    (test-equal "Renderer returns expected style settings position/top/left"
      "style='position:absolute; top:10px; left:10px' href=''></a>\n"
      (string-drop (list-ref retval 2) 22))

    (test-equal "Renderer returns expected canvas"
      "<canvas id='chart-'></canvas>\n"
      (string-append (string-take (list-ref retval 3) 18) (string-drop (list-ref retval 3) 26)))

    (test-equal "Renderer returns expected header end"
      "</div>\n"
      (list-ref retval 4))

    (test-equal "Renderer returns expected body start"
      "<script id='script-"
      (string-take (list-ref retval 5) 19))

    (test-equal "Renderer returns expected all_ticks"
      "var all_ticks = [\"date1\", \"date2\", \"date3\"];\n"
      (list-ref retval 6))

    (test-equal "Renderer returns expected var url1"
      "var url1 = false;\n"
      (list-ref retval 7))

    (test-equal "Renderer returns expected chart type"
      "var charttype = \"bar\";\n"
      (list-ref retval 8))

    (test-equal "Renderer returns chart ID"
      "var chartid = 'chart-"
      (string-take (list-ref retval 9) 21))

    (test-equal "Renderer returns expected yAxis Display"
      "chartjsoptions.options.scales.yAxes[0].ticks.callback = yAxisDisplay;\n"
      (list-ref retval 11))

    (test-equal "Renderer returns expected xAxis Display"
      "chartjsoptions.options.scales.xAxes[0].ticks.callback = xAxisDisplay;\n"
      (list-ref retval 12))

    (test-equal "Renderer returns expected Tool Tip Label"
      "chartjsoptions.options.tooltips.callbacks.label = tooltipLabel;\n"
      (list-ref retval 13))

    (test-equal "Renderer returns expected Tool Tip Title"
      "chartjsoptions.options.tooltips.callbacks.title = tooltipTitle;\n"
      (list-ref retval 14))

    (test-equal "Renderer returns expected default Font"
      "Chart.defaults.global.defaultFontFamily = \"'Trebuchet MS', Arial, Helvetica, sans-serif\";\n"
      (list-ref retval 15))

    (test-equal "Renderer returns expected function yAxisDisplay"
      "function yAxisDisplay(value,index,values)"
      (string-take (list-ref retval 16) 41))

    (test-equal "Renderer returns expected function xAxisDisplay"
      "function xAxisDisplay(value,index,values) { return all_ticks[index]; };\n\n"
      (list-ref retval 17))

    (test-equal "Renderer returns expected function tooltiplabel"
      "function tooltipLabel(tooltipItem,data)"
      (string-take (list-ref retval 18) 39))

    (test-equal "Renderer returns expected function tooltipTitle"
      "function tooltipTitle(array,data) {\n                    return all_ticks[array[0].index]; }\n\n"
      (list-ref retval 19))

    (test-equal "Renderer returns expected background color 1"
      "// draw the background color\nChart.pluginService.register({\n  beforeDraw: function (chart, easing) {\n    if (chart.config.options.chartArea && chart.config.options.chartArea.backgroundColor) {\n      var ctx = chart.chart.ctx;\n      var chartArea = chart.chartArea;\n      ctx.save();\n      ctx.fillStyle = chart.config.options.chartArea.backgroundColor;\n      ctx.fillRect(chartArea.left, chartArea.top, chartArea.right - chartArea.left, chartArea.bottom - chartArea.top);\n      ctx.restore();\n    }\n  }\n})\n\ndocument.getElementById(chartid).onclick = function(evt) {\n  var activepoints = myChart.getElementAtEvent(evt);\n  var anchor = document.getElementById('jump-"
      (string-take (list-ref retval 20) 664))

    (test-equal "Renderer returns expected background color 2"
      "');\n  if (activepoints.length > 0) {\n    var index = activepoints[0]['_index'];\n    var datasetIndex = activepoints[0]['_datasetIndex'];\n    if (charttype === 'pie') {\n      var url = url1[index];\n    } else {\n      var url = url1[datasetIndex];\n    }\n    anchor.href = url;\n    // anchor.style = 'position:absolute; top:' + (evt.clientY - 30) + 'px; left:' + evt.clientX + 'px';\n    anchor.textContent = 'Load ' + url.substring(4, url.indexOf(':'));\n  } else {\n    anchor.href = '';\n    anchor.textContent = '';\n  }\n}\n\n"
      (string-drop (list-ref retval 20) 672))
      ;;(string-append (string-take (list-ref retval 20) 18) (string-drop (list-ref retval 20) 26)))

    (test-equal "Renderer returns the chart instance"
      "var myChart = new Chart(chartid, chartjsoptions);\n"
      (list-ref retval 21))

    (test-equal "Renderer returns expected Close body"
      "</script>"
      (list-ref retval 22)))

  (test-end "HTML Generic Chart Rendering")
)

;; -----------------------------------------------------------------------

(define (test-bar-chart-rendering)

  (test-begin "HTML Bar Chart Rendering")

  (let* (
         (chart (gnc:make-html-chart))
         (retval ((lambda (c) 
                    (gnc:html-chart-set-type! c 'bar)
                    (gnc:html-chart-set-row-labels! c '("date1" "date2" "date3"))
                    (gnc:html-chart-set-col-labels! c '("dataset1" "dataset2" "dataset3"))
                    (gnc:html-chart-set-data! chart '((25 45 30)))
                    (gnc:html-chart-append-row! c '(65 50 40))
                    (gnc:html-chart-append-row! c '(75 55 70))
                    (gnc:html-chart-set-colors! chart '("red" "green" "blue"))
                    (reverse (gnc:html-chart-render c #f)))
                  chart))
       )

    (test-equal "Bar Chart - Check chart options"
      "var chartjsoptions = \n{\n    \"type\" : \"bar\",\n    \"options\" : \n    {\n        \"maintainAspectRatio\" : false,\n        \"chartArea\" : \n        {\n            \"backgroundColor\" : \"#fffdf6\"\n        },\n        \"legend\" : \n        {\n            \"position\" : \"right\",\n            \"reverse\" : true,\n            \"labels\" : \n            {\n                \"fontColor\" : \"black\"\n            }\n        },\n        \"elements\" : \n        {\n            \"line\" : \n            {\n                \"tension\" : 0\n            },\n            \"point\" : \n            {\n                \"pointStyle\" : true\n            }\n        },\n        \"tooltips\" : \n        {\n            \"callbacks\" : \n            {\n                \"label\" : false\n            }\n        },\n        \"scales\" : \n        {\n            \"xAxes\" : [\n                {\n                    \"display\" : true,\n                    \"type\" : \"category\",\n                    \"distribution\" : \"series\",\n                    \"offset\" : true,\n                    \"gridLines\" : \n                    {\n                        \"display\" : true,\n                        \"lineWidth\" : 1.5\n                    },\n                    \"scaleLabel\" : \n                    {\n                        \"display\" : false,\n                        \"labelString\" : false\n                    },\n                    \"ticks\" : \n                    {\n                        \"fontSize\" : 12,\n                        \"maxRotation\" : 30\n                    }\n                }],\n            \"yAxes\" : [\n                {\n                    \"stacked\" : false,\n                    \"display\" : true,\n                    \"gridLines\" : \n                    {\n                        \"display\" : true,\n                        \"lineWidth\" : 1.5\n                    },\n                    \"scaleLabel\" : \n                    {\n                        \"display\" : false,\n                        \"labelString\" : false\n                    },\n                    \"ticks\" : \n                    {\n                        \"fontSize\" : 10,\n                        \"beginAtZero\" : false\n                    }\n                }]\n        },\n        \"title\" : \n        {\n            \"display\" : false,\n            \"fontSize\" : 16,\n            \"fontStyle\" : \"\",\n            \"text\" : false\n        }\n    },\n    \"data\" : \n    {\n        \"labels\" : [1, 2, 3],\n        \"datasets\" : [\n            {\n                \"label\" : \"dataset1\",\n                \"data\" : [75, 65, 25],\n                \"backgroundColor\" : \"red\",\n                \"stack\" : \"0\",\n                \"fill\" : false,\n                \"showLine\" : true,\n                \"borderColor\" : \"red\",\n                \"lineTension\" : 0\n            }, \n            {\n                \"label\" : \"dataset2\",\n                \"data\" : [55, 50, 45],\n                \"backgroundColor\" : \"green\",\n                \"stack\" : \"1\",\n                \"fill\" : false,\n                \"showLine\" : true,\n                \"borderColor\" : \"green\",\n                \"lineTension\" : 0\n            }, \n            {\n                \"label\" : \"dataset3\",\n                \"data\" : [70, 40, 30],\n                \"backgroundColor\" : \"blue\",\n                \"stack\" : \"2\",\n                \"fill\" : false,\n                \"showLine\" : true,\n                \"borderColor\" : \"blue\",\n                \"lineTension\" : 0\n            }]\n    }\n};\n\n"
      (list-ref retval 10)))

  (test-end "HTML Bar Chart Rendering")
)

;; -----------------------------------------------------------------------

(define (test-pie-chart-rendering)

  (test-begin "HTML Pie Chart Rendering")

  (let* (
         (chart (gnc:make-html-chart))
         (retval ((lambda (c) 
                    (gnc:html-chart-set-type! c 'pie)
                    (gnc:html-chart-set-row-labels! c '("date1" "date2" "date3"))
                    (gnc:html-chart-set-data! chart '(25 45 30))
                    (gnc:html-chart-set-colors! chart '("red" "green" "blue"))
                    (reverse (gnc:html-chart-render c #f)))
                  chart))
       )

    (test-equal "Pie Chart - Check chart options"
      "var chartjsoptions = \n{\n    \"type\" : \"pie\",\n    \"options\" : \n    {\n        \"maintainAspectRatio\" : false,\n        \"chartArea\" : \n        {\n            \"backgroundColor\" : \"#fffdf6\"\n        },\n        \"legend\" : \n        {\n            \"position\" : \"right\",\n            \"reverse\" : true,\n            \"labels\" : \n            {\n                \"fontColor\" : \"black\"\n            }\n        },\n        \"elements\" : \n        {\n            \"line\" : \n            {\n                \"tension\" : 0\n            },\n            \"point\" : \n            {\n                \"pointStyle\" : true\n            }\n        },\n        \"tooltips\" : \n        {\n            \"callbacks\" : \n            {\n                \"label\" : false\n            }\n        },\n        \"scales\" : \n        {\n            \"xAxes\" : [\n                {\n                    \"display\" : false,\n                    \"type\" : \"category\",\n                    \"distribution\" : \"series\",\n                    \"offset\" : false,\n                    \"gridLines\" : \n                    {\n                        \"display\" : true,\n                        \"lineWidth\" : 1.5\n                    },\n                    \"scaleLabel\" : \n                    {\n                        \"display\" : false,\n                        \"labelString\" : false\n                    },\n                    \"ticks\" : \n                    {\n                        \"fontSize\" : 12,\n                        \"maxRotation\" : 30\n                    }\n                }],\n            \"yAxes\" : [\n                {\n                    \"stacked\" : false,\n                    \"display\" : false,\n                    \"gridLines\" : \n                    {\n                        \"display\" : true,\n                        \"lineWidth\" : 1.5\n                    },\n                    \"scaleLabel\" : \n                    {\n                        \"display\" : false,\n                        \"labelString\" : false\n                    },\n                    \"ticks\" : \n                    {\n                        \"fontSize\" : 10,\n                        \"beginAtZero\" : false\n                    }\n                }]\n        },\n        \"title\" : \n        {\n            \"display\" : false,\n            \"fontSize\" : 16,\n            \"fontStyle\" : \"\",\n            \"text\" : false\n        }\n    },\n    \"data\" : \n    {\n        \"labels\" : [\"date1\", \"date2\", \"date3\"],\n        \"datasets\" : [\n            {\n                \"data\" : [25, 45, 30],\n                \"backgroundColor\" : [\"red\", \"green\", \"blue\"]\n            }]\n    }\n};\n\n"
      (list-ref retval 10)))

  (test-end "HTML Pie Chart Rendering")
)

;; -----------------------------------------------------------------------

(define (test-scatter-chart-rendering)

  (test-begin "HTML Scatter Chart Rendering")

  (let* (
         (chart (gnc:make-html-chart))
         (retval ((lambda (c) 
                    (gnc:html-chart-set-type! c 'scatter)
                    (gnc:html-chart-set-row-labels! c '("date1" "date2" "date3"))
                    (gnc:html-chart-set-data! chart '((25 45)))
                    (gnc:html-chart-set-colors! chart '("red" "green" "blue"))
                    (reverse (gnc:html-chart-render c #f)))
                  chart))
       )

    (test-equal "Scatter - Check chart options"
      "var chartjsoptions = \n{\n    \"type\" : \"line\",\n    \"options\" : \n    {\n        \"maintainAspectRatio\" : false,\n        \"chartArea\" : \n        {\n            \"backgroundColor\" : \"#fffdf6\"\n        },\n        \"legend\" : \n        {\n            \"position\" : \"right\",\n            \"reverse\" : true,\n            \"labels\" : \n            {\n                \"fontColor\" : \"black\"\n            }\n        },\n        \"elements\" : \n        {\n            \"line\" : \n            {\n                \"tension\" : 0\n            },\n            \"point\" : \n            {\n                \"pointStyle\" : true\n            }\n        },\n        \"tooltips\" : \n        {\n            \"callbacks\" : \n            {\n                \"label\" : false\n            }\n        },\n        \"scales\" : \n        {\n            \"xAxes\" : [\n                {\n                    \"display\" : true,\n                    \"type\" : \"linear\",\n                    \"distribution\" : \"linear\",\n                    \"offset\" : true,\n                    \"gridLines\" : \n                    {\n                        \"display\" : true,\n                        \"lineWidth\" : 1.5\n                    },\n                    \"scaleLabel\" : \n                    {\n                        \"display\" : false,\n                        \"labelString\" : false\n                    },\n                    \"ticks\" : \n                    {\n                        \"fontSize\" : 12,\n                        \"maxRotation\" : 30\n                    }\n                }],\n            \"yAxes\" : [\n                {\n                    \"stacked\" : false,\n                    \"display\" : true,\n                    \"gridLines\" : \n                    {\n                        \"display\" : true,\n                        \"lineWidth\" : 1.5\n                    },\n                    \"scaleLabel\" : \n                    {\n                        \"display\" : false,\n                        \"labelString\" : false\n                    },\n                    \"ticks\" : \n                    {\n                        \"fontSize\" : 10,\n                        \"beginAtZero\" : false\n                    }\n                }]\n        },\n        \"title\" : \n        {\n            \"display\" : false,\n            \"fontSize\" : 16,\n            \"fontStyle\" : \"\",\n            \"text\" : false\n        }\n    },\n    \"data\" : \n    {\n        \"datasets\" : [\n            {\n                \"data\" : [\n                    {\n                        \"x\" : 25,\n                        \"y\" : 45\n                    }],\n                \"fill\" : false,\n                \"backgroundColor\" : [\"red\", \"green\", \"blue\"],\n                \"pointBorderColor\" : [\"red\", \"green\", \"blue\"],\n                \"pointBorderWidth\" : 3,\n                \"borderColor\" : \"rgb(84,182,182)\"\n            }]\n    }\n};\n\n"
      (list-ref retval 10)))

  (test-end "HTML Scatter Chart Rendering")
)


