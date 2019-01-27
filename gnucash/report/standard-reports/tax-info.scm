;; -*-scheme-*-

;; ------------------------------------------------------------------
;; Top-level definitions
;; ------------------------------------------------------------------

(define-module (gnucash report standard-reports tax-info))
;;(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash core-utils)) ; for gnc:version
(use-modules (gnucash utilities))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gnome-utils))
(use-modules (sw_gnome_utils)) ;; to get to gnc-ui-account-get-tax-form-line
(use-modules (gnucash gettext)) ;; needed for the (N_ ) translation, changed between 2.6.4 and 2.6.14
(gnc:module-load "gnucash/report/report-system" 0)

;; ------------------------------------------------------------------
;; Top-level definitions
;; ------------------------------------------------------------------

(define report-name (N_ "Tax Info Report"))
(define report-version "1.0")

(use-modules (srfi srfi-9)) ;; for define-record-type, so called "SRFI-9 Records"
(define-record-type <tax-editor-info>
  (make-tax-editor-info account)
  tax-editor-info?
  (account tax-editor-info-account set-tax-editor-info-account!)
  (country tax-editor-info-country set-tax-editor-info-country!)
  (type    tax-editor-info-type    set-tax-editor-info-type!)
  (year    tax-editor-info-year    set-tax-editor-info-year!)
  (form    tax-editor-info-form    set-tax-editor-info-form!)
  (item    tax-editor-info-item	   set-tax-editor-info-item!)
  (line    tax-editor-info-line    set-tax-editor-info-line!)
  (column  tax-editor-info-column  set-tax-editor-info-column!)
)

;; ------------------------------------------------------------------
;; Define the Options for this report
;; ------------------------------------------------------------------

(define optname-tax-year (N_ "Tax Year"))

(define (options-generator)
  (let* ((options (gnc:new-options)))

    ;; ---------------------------------------------------------------------------
    ;; GENERAL page
    ;; ---------------------------------------------------------------------------

    ;; tax year as a string option
    (gnc:register-option
      options
      (gnc:make-string-option
         gnc:pagename-general                        ;; page name
         optname-tax-year                            ;; option name
         "a"                                         ;; position within the page
         (N_ "A four digit number greater 1900")     ;; help text
         "2013"                                      ;; default value
      )
    )

    options ;; RETURN value

  )
)

;; ------------------------------------------------------------------
;; fill the tax-editor-info record from the tax ID string
;; ------------------------------------------------------------------

(define create-tax-id-record
  (lambda (account tax-id-string tax-year)

   (define accessor-types (vector 'country 'type 'form 'item 'column))
   ;; DO NOT ADD 'line and 'year!
   ;; Note that 'line is missing by intention, it will be handled separately.

    ;; ------------------------------------------
    ;; local helper function
    ;; ------------------------------------------
    (define select-field-accessor
      (lambda (group-type)
        (cond
          ((eqv? group-type 'country) set-tax-editor-info-country!)
          ((eqv? group-type 'type)    set-tax-editor-info-type!)
          ((eqv? group-type 'form)    set-tax-editor-info-form!)
          ((eqv? group-type 'item)    set-tax-editor-info-item!)
          ((eqv? group-type 'column)  set-tax-editor-info-column!)
          (else #f)
        )
      )
    )

    ;; ------------------------------------------
    ;; local helper function
    ;; ------------------------------------------
    (define get-first-delimiter-position
      (lambda (source-string delimiter-character)
        ;; --------------------------------------
        ;; run the delimiter search as recursion
        ;; --------------------------------------
        (define check-position
          (lambda (source-string position delimiter-character)
            (if (eq? position (string-length source-string))
              #f
              (if (char=? (string-ref source-string position) delimiter-character)
	        position
	        (check-position source-string (+ position 1) delimiter-character)
              )
            )
          )
        )
        (check-position source-string 0 delimiter-character)
      )
    )
    ;; ------------------------------------------
    ;; local helper function
    ;; ------------------------------------------
    (define set-tax-record-fields
      (lambda (tax-record field source-string)
        (let ((delimiter-position (get-first-delimiter-position source-string #\:)))
          (cond
            (
              (and
                (eq? delimiter-position #f)
                (< field (- (vector-length accessor-types) 1))
              )
              #f ;; terminate recursion and RETURN value, not a valid source string
            )
            (
              (eq? delimiter-position #f)
              ((select-field-accessor (vector-ref accessor-types field))
                  tax-record
                  source-string)
              #t ;; terminate recursion and RETURN value, source string evaluation complete
            )
            (
              else
              ((select-field-accessor (vector-ref accessor-types field))
                  tax-record
                  (string-take source-string delimiter-position)
              )
              (set-tax-record-fields ;; go into next recursion
                tax-record
                (+ 1 field)
                (string-drop source-string (+ 1 delimiter-position))
              )
            )
          )
        )
      )
    )

    ;; ------------------------------------------
    ;; local main loop
    ;; ------------------------------------------
    (let (
           (source-string         tax-id-string                 )
           (delimiter-position    0                             )
           (tax-account-record    (make-tax-editor-info account))
         )
      (gnc:debug "TAX-ID" " Source String: " source-string)
      ;; --------------------------------------
      ;; fill the tax record fields except for year and line
      ;; if this brings a reasonable result, fill the rest
      ;; --------------------------------------
      (cond
        (
          (eq? (set-tax-record-fields tax-account-record 0 source-string) #t)
          ;; --------------------------------------
          ;; fill the tax year field
          ;; --------------------------------------
          (set-tax-editor-info-year! tax-account-record (string->number tax-year))
          ;; --------------------------------------
          ;; fill the tax line field
          ;; --------------------------------------
          (set-tax-editor-info-line!
            tax-account-record
            (gnc-ui-account-get-tax-form-line
              tax-year
              (tax-editor-info-country tax-account-record)
              (tax-editor-info-type    tax-account-record)
              (tax-editor-info-form    tax-account-record)
              (tax-editor-info-item    tax-account-record)
            )
          )
        )
        (
          else
          (set-tax-editor-info-year! tax-account-record #f)
          (set-tax-editor-info-line! tax-account-record #f)
        )
      )

      (gnc:debug "TAX-ID" " Record: " tax-account-record)
      tax-account-record ;; RETURN value
    )
  )
)

;; ------------------------------------------------------------------
;; helper function
;; ------------------------------------------------------------------

(define key-sort
  (lambda (list-unsorted)

    ;; -----------------------------------
    ;; only string sorting is needed
    ;; -----------------------------------
    (if
      (not (string? (caar list-unsorted)))
      list-unsorted ;; RETURN with nothing done

      ;; else
      (let (
             (list-sorted (list))
             (list-keys (list))
           )
        ;; -----------------------------------
        ;; get all keys from the unsorted list
        ;; -----------------------------------
        (for-each
          (lambda (listelement)
            (set! list-keys (cons (car listelement) list-keys))
          )
          list-unsorted
        )
        (gnc:debug "TAX-ID" " Key List: " list-keys)
        ;; -----------------------------------
        ;; go through the sorted key lists
        ;; -----------------------------------
        (for-each
          (lambda (key)
            ;; -----------------------------------------------
            ;; go through the unsorted list, fetch the element
            ;; with the key and store it in the sorted list
            ;; -----------------------------------------------
            (for-each
              (lambda (listelement)
                (if (string=? key (car listelement))
                  (set! list-sorted (cons listelement list-sorted))
                )
              )
              list-unsorted
            )
          )
          (sort list-keys string<?) ;; here is where the sorting happens
        )
        list-sorted ;; RETURN value
      )
    )
  )
)

;; ------------------------------------------------------------------
;; helper function sub-group-record-list-simple
;;
;; Input: A plain and unsorted list of tax records
;; (
;;   tax-record_1
;;   tax-record_2
;;   ...
;;   tax-record_n
;; )
;;
;; Output: The list of records in groups of records that have the same
;; field value in a given field
;; Example: Sub-group the tax records according to tax country
;; (
;;   (country-code_1 (
;;     tax-record_1
;;     tax-record_2
;;     ...
;;     tax-record_m)
;;   )
;;   (country-code_2 (
;;     tax-record_1
;;     tax-record_2
;;     ...
;;     tax-record_n)
;;   )
;;   ...
;;   (country-code_i (<list of tax records with country-code_i>))
;; )
;; ------------------------------------------------------------------

(define sub-group-record-list-simple
  (lambda (tax-record-list group-type)

    ;; ------------------------------------------
    ;; local helper function
    ;; ------------------------------------------
    (define select-field-accessor
      (lambda (group-type)
        (cond
          ((eqv? group-type 'country) tax-editor-info-country)
          ((eqv? group-type 'type)    tax-editor-info-type)
          ((eqv? group-type 'form)    tax-editor-info-form)
          ((eqv? group-type 'item)    tax-editor-info-item)
          ((eqv? group-type 'line)    tax-editor-info-line)
          ((eqv? group-type 'column)  tax-editor-info-column)
          (else #f)
        )
      )
    )

    ;; ------------------------------------------
    ;; local helper function
    ;; e.g. list of all available tax countries
    ;; e.g. list of all available tax types
    ;; etc.
    ;; ------------------------------------------

    (define get-list-of-field-values
      (lambda (tax-record-list field-accessor)
        (let* (
                (result-record-list (list))
              )
          (for-each
            (lambda (tax-record)
              (let (
                     (tax-field (field-accessor tax-record))
                   )
                (if (and
                      (not (eqv? tax-field #f))
                      (eqv? (member tax-field result-record-list) #f)
                    )
                  (set! result-record-list (cons tax-field result-record-list))
                )
              )
            )
            tax-record-list
          )
          result-record-list ;; RETURN value
        )
      )
    )

    ;; ------------------------------------------
    ;; local helper function
    ;; return a list of records that have the same
    ;; field value in a given field
    ;; ------------------------------------------
    (define list-records-of-same-field-value
      (lambda (tax-record-list field-accessor field-value)
        (let (
               (result-list (list))
             )
          (for-each
            (lambda (tax-record)
              (if (equal? (field-accessor tax-record) field-value)
                (set! result-list (cons tax-record result-list))
              )
            )
            tax-record-list
          )
          result-list ;; RETURN value
        )
      )
    )

    ;; ------------------------------------------
    ;; local main loop
    ;; Input 1: a list of tax records
    ;; Input 2: an identifier for a wanted field (e.g. tax form)
    ;; Output:
    ;; Tax records grouped by field values in a list with following format:
    ;; (
    ;;   (tax-form_1 (record_1 recored_2 ... recored_n)) <- all these records have the same value "tax-form_1" in field "form"
    ;;   (tax-form_2 (record_1 recored_2 ... recored_m)) <- all these records have the same value "tax-form_2" in field "form"
    ;;   ...
    ;;   (tax-form_i (record_1 recored_2 ... recored_k)) <- all these records have the same value "tax-form_i" in field "form"
    ;; )
    ;; ------------------------------------------
    (let* (
            (new-tax-record-list (list))
            ;; ------------------------------------------
            ;; find out which field is to be processed
            ;; ------------------------------------------
            (field-accessor (select-field-accessor group-type))
            ;; ------------------------------------------
            ;; find out which field values there are
            ;; ------------------------------------------
            (field-value-list (get-list-of-field-values tax-record-list field-accessor))
          )
      (for-each
        (lambda (field-value)
          (set! new-tax-record-list
            (cons
              (list
                field-value
                (list-records-of-same-field-value tax-record-list field-accessor field-value)
              )
              new-tax-record-list
            )
          )
        )
        field-value-list
      )
      ;;new-tax-record-list ;; RETURN value
      (key-sort new-tax-record-list) ;; RETURN value
    )
  )
)

;; ------------------------------------------------------------------
;; helper function
;;
;; group the list according to tax record fields
;; (
;;   country-code_1 (
;;     tax-type_1 (
;;       tax-form_1 (
;;         tax-item_1 (
;;           tax-column_1 (
;;             tax-record_1
;;             tax-record_2
;;             ...
;;             tax-record_i)
;;           )
;;           tax-column_2 (
;;             tax-record_1
;;             tax-record_2
;;             ...
;;             tax-record_j)
;;           )
;;           ...
;;           tax-column_k (...)
;;         )
;;         tax-item_2 (...)
;;         ...
;;         tax-item_l (...)
;;       )
;;       tax-form_2 (...)
;;       ...
;;       tax-form_m (...)
;;     )
;;     tax-type_2 (...)
;;     ...
;;     tax-type_n (...)
;;   )
;;   country-code_2 (...)
;;   ...
;;   country-code_q (...)
;; )
;; Note that the tax records still keep the complete
;; information even if this means double information
;; ------------------------------------------------------------------

(define tax-record-list-grouped
  (lambda (tax-record-list)
    (let (
           (recursion-levels (vector 'country 'type 'form 'line 'column))
           (recursion-level 0)
         )

      ;; --------------------------------------
      ;; recursion level by level
      ;; --------------------------------------
      (define group-level
        (lambda (record-list level)
          (let (
                 (tmp-record-list (list))
                )
            (cond
              (
                (eq? level (vector-length recursion-levels))
                record-list ;; recursion finished
              )
              (
                else
                (set! record-list (sub-group-record-list-simple record-list (vector-ref recursion-levels level)))
                (for-each
                  (lambda (record-group)
                    (set! tmp-record-list
                      (cons
                        (list
                          (car record-group)
                          (group-level (cadr record-group) (+ 1 level))
                        )
                        tmp-record-list
                      )
                    )
                  )
                  record-list
                )
                tmp-record-list ;; RETURN value
              )
            )
          )
        )
      )

      ;; --------------------------------------
      ;; local main loop
      ;; --------------------------------------
      (set! tax-record-list (group-level tax-record-list recursion-level))
    )
    (gnc:debug "TAX-ID" " Tax record list: " tax-record-list)
    tax-record-list ;; RETURN value
  )
)

;; ------------------------------------------------------------------
;; helper function
;; ------------------------------------------------------------------

(define get-tax-record-list
  (lambda (account-list tax-year)
    (map
      (lambda (account)
        (create-tax-id-record account (xaccAccountGetTaxUSCode account) tax-year)
      )
      account-list
    )
  )
)

;; ------------------------------------------------------------------
;; helper function
;; ------------------------------------------------------------------

(define filter-all-tax-related-accounts
  (lambda (account-list)
    (cond
      (
        (null? account-list)
        account-list
      )
      (
        (xaccAccountGetTaxRelated (car account-list))
        (cons (car account-list) (filter-all-tax-related-accounts (cdr account-list)))
      )
      (else
        (filter-all-tax-related-accounts (cdr account-list))
      )
    )
  )
)

;; ------------------------------------------------------------------
;; Render the HTML document
;; ------------------------------------------------------------------

(define (document-renderer report-obj)

  ;; -------------------------------------
  ;; helper funtion to access the options
  ;; -------------------------------------
  (define (get-option-val pagename optname)
    (gnc:option-value
     (gnc:lookup-option
      (gnc:report-options report-obj) pagename optname)))

  (let* (
          ;; ------------------------------------------
          ;; declare the output document
          ;; ------------------------------------------
          (document (gnc:make-html-document))

          ;; ------------------------------------------
          ;; get a list of all available accounts
          ;; ------------------------------------------
          (account-list (gnc-account-get-descendants-sorted (gnc-get-current-root-account)))

          ;; ------------------------------------------
          ;; read the tax year from the options
          ;; ------------------------------------------
          (tax-year (get-option-val gnc:pagename-general  optname-tax-year))
        )

    ;; ------------------------------------------------------------------
    ;; validate the tax year option
    ;; ------------------------------------------------------------------
    (if (or
          (not (string->number tax-year))
          (<= (string->number tax-year) 1900)
          (>= (string->number tax-year) 10000)
        )
      (gnc:html-document-add-object!
        document
        (gnc:make-html-text
          (gnc:html-markup-br)
            (N_ "Not a valid tax year.")
          (gnc:html-markup-br)
        )
      )

      ;; else continue, tax year is valid
      ;; ------------------------------------------------------------------
      ;; process the tax-year
      ;; ------------------------------------------------------------------
      (let* (
              ;; -----------------------------------
              ;; get the start date and time pair
              ;; -----------------------------------
              (start-date
                (let (
                       (zero-date (localtime 0))
                     )
                  (set-tm:year zero-date (- (string->number tax-year) 1900))
                  zero-date
                )
              )
              ;;(start-tp (cons (car (mktime start-date)) 0))
              (start-sec (car (mktime start-date)) )

              ;; -----------------------------------
              ;; get the stop date and time pair
              ;; -----------------------------------
              (stop-date
                (let (
                       (stop-date start-date)
                     )
                  (set-tm:mday stop-date (+ (gnc:days-in-year (string->number tax-year)) 1))
                  stop-date
                )
              )
              ;;(stop-tp  (cons (car (mktime  stop-date)) 0))
              (stop-sec (car (mktime start-date)) )
            )

        ;; ------------------------------------------------
        ;; filter the account list for tax related accounts
        ;; ------------------------------------------------
        (set! account-list (filter-all-tax-related-accounts account-list))

        (if (null? account-list)
          (gnc:html-document-add-object!
            document
            (gnc:make-html-text
              (gnc:html-markup-br)
                (N_ "No tax related accounts avialable.")
              (gnc:html-markup-br)
            )
          )

          ;; else continue, there is at least one tax related account
          ;; --------------------------------------------------------
          ;; process the list of tax related accounts
          ;; --------------------------------------------------------
          (let* (
                 ;; ------------------------------------------------
                 ;; create the tax record list from the account list
                 ;; ------------------------------------------------
                 (tax-record-list
                   (tax-record-list-grouped
                     (get-tax-record-list account-list tax-year)
                   )
                 )
               )

            (for-each
              (lambda (country-group)

                ;; ---------------------------------------------------
                ;; print out the country that is going to be processed
                ;; ---------------------------------------------------
                (gnc:html-document-add-object!
                  document
                  (gnc:make-html-text
                    (gnc:html-markup-h2
                      "Country: " (car country-group))
                    ;;(gnc:html-markup-br)
                  )
                )

                (for-each
                  (lambda (taxtype-group)

                    ;; ----------------------------------------------------
                    ;; print out the tax type that is going to be processed
                    ;; ----------------------------------------------------
                    (gnc:html-document-add-object!
                      document
                      (gnc:make-html-text
                        (gnc:html-markup-h2
                          (car taxtype-group) " " tax-year " (" (qof-print-date start-sec) " - " (qof-print-date (- stop-sec 10800)) " )")
                      )
                    )

                    ;; ----------------------------------------------------
                    ;; create the table that will hold the output data
                    ;; ----------------------------------------------------

                    (let (
                           (result-table (gnc:make-html-table))
                           (row 0)
                           (col 0)
                         )

                      (gnc:html-document-add-object!
                        document
                        result-table
                      )

                      ;; ----------------------------------------------------
                      ;; and fill it with data
                      ;; ----------------------------------------------------

                      (for-each
                        (lambda (taxform-group)

                          ;; -----------------------------------
                          ;; add a row for the name of this form
                          ;; -----------------------------------
                          (gnc:html-table-append-row!
                            result-table
                            (list (car taxform-group))
                          )

                          (for-each
                            (lambda (taxline-group)
                              (let (
                                     (totals-line (list))
                                   )

                                ;; -------------------------------------
                                ;; add the heading row for this tax line
                                ;; -------------------------------------
                                (gnc:html-table-append-row!
                                  result-table
                                  (append
                                    ;; -------------------------------
                                    ;; the item name for this tax line
                                    ;; -------------------------------
                                    (list
                                      " " ;; leave the first column empty
                                      " " ;; leave the second column emtpy
                                      " " ;; leave the third column emtpy
                                      (tax-editor-info-item    ;; the item name of
                                        (caadar                ;; the first tax record of
                                          (cadr taxline-group) ;; the first tax-column group
                                        )
                                      )
                                    )
                                    ;; ------------------------------------------------
                                    ;; the names for this tax columns for this tax line
                                    ;; ------------------------------------------------
                                    (map
                                      (lambda (taxcolumn-group)
                                        (car taxcolumn-group)
                                      )
                                      (cadr taxline-group) ;; list of columns for this line
                                    )
                                  )
                                )

                                (set! col 0)
                                (for-each
                                  (lambda (taxcolumn-group)
                                    (let (
                                           (column-total 0.0)
                                         )
                                      (for-each
                                        (lambda (tax-record)
                                          (let* (
                                                  (account (tax-editor-info-account tax-record))
                                                  (record-balance
                                                    (gnc:account-get-amount-interval
                                                      account
                                                      start-sec
                                                      stop-sec
                                                      #f ;; include-children?
                                                      ;;#t ;; sign?
                                                      (eq? ;; sign?
                                                         ACCT-TYPE-INCOME
                                                         (xaccAccountGetType account)
                                                      )
                                                    )
                                                  )
                                                )
                                            (set! column-total
                                              (+ column-total record-balance)
                                            )
                                            ;; ----------------------------------------------------
                                            ;; add a row for each account assigned to this tax line
                                            ;; and place it in the correct column
                                            ;; ----------------------------------------------------
                                            (gnc:html-table-append-row!
                                              result-table
                                              (append
                                                (list
                                                  " "
                                                  " "
                                                  " "
                                                  (gnc:make-html-text
                                                    (gnc:html-markup-anchor
                                                      (gnc:account-anchor-text account)
                                                      ;;(gnc-account-get-full-name account)
                                                      (xaccAccountGetName account)
                                                    )
                                                  )
                                                )
                                                (vector->list(make-vector col " "))
                                                (list record-balance)
                                              )
                                            )
                                          ) ;; end of let record-balance
                                        )
                                        (cadr taxcolumn-group)
                                      )
                                      (set! col (+ 1 col))
                                      (set! totals-line (append totals-line (list column-total)))
                                    ) ;; end of let column-total
                                  )
                                  (cadr taxline-group)
                                )

                                ;; ----------------------------------------------------
                                ;; add the row for the total value of this this tax line
                                ;; ----------------------------------------------------
                                (gnc:html-table-append-row! result-table
                                  (append
                                    (list
                                      " "
                                      (car taxline-group)
                                      (let((total-sum 0))
                                        (for-each
                                          (lambda (col-sum)
                                            (set! total-sum (+ col-sum total-sum))
                                          )
                                          totals-line
                                        )
                                        total-sum ;; RETURN value (here it becomes a list element)
                                      )
                                      "Total"
                                    )
                                    totals-line ;; total per column
                                  )
                                )
                                (gnc:html-table-append-row! result-table (list " " )) ;; empty seperation row

                              ) ;; end of let total-line

                            )
                            (cadr taxform-group)
                          )
                        )
                        (cadr taxtype-group)
                      )
                    )
                  )
                  (cadr country-group)
                )
              )
              tax-record-list
            )

          ) ;; end of processing the list of tax related accounts

        ) ;; end of tax account list validation

      ) ;; end of processing the tax year

    ) ;; end of tax your validation



    ;; ------------------------------------------
    ;; append the report footer
    ;; ------------------------------------------
    (gnc:html-document-add-object!
      document
      (gnc:make-html-text
        (gnc:html-markup-br)
        (gnc:html-markup-i
          (N_ "Report Creation Date: ")
          (qof-print-date (gnc:get-today))
        )
        (gnc:html-markup-br)
        (gnc:html-markup-i
          report-name
          " "
          report-version
        )
        (gnc:html-markup-br)
        (gnc:html-markup-i
          "GnuCash "
          gnc:version
        )
      )
    )

    document ;; RETURN value

  )
)

;; ------------------------------------------------------------------
;; Define the actual report
;; ------------------------------------------------------------------

(gnc:define-report
 'version 1
 'name report-name
 'report-guid "8419975cc04241dfb45c8f1c81ef8e18"
 'menu-tip (N_ "Tax Declaration Info")
 ;;'menu-path (list gnc:menuname-utility)
 'options-generator options-generator
 'renderer document-renderer)
