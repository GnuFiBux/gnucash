;; -*-scheme-*-

;; ------------------------------------------------------------------
;; GNUCASH ASSET PERFORMANCE REPORT
;; ------------------------------------------------------------------

;; What is this report doing?
;; This report shall help to evaluate financial products that do not
;; explicitly state the achieved interest rate in their reports.

;; Example 1:
;; A company offers a product for a private old-age pension to its
;; customers.
;; As part of the products contract a monthly amount of money is received
;; from the customer until the customer retires.
;; The received money is managed/invested by the company on behalf
;; of the customer e.g. based on shares.
;; At the time of retirement, the result of the investment is payed
;; out to the customer (or any kind of payback based on the achieved result).
;; Problem:
;; The company does not guarantee any gain, it just provides the customer
;; with prospects according to "market analysis". No fixed interest rates
;; have been agreed or committed. All you get as a yearly statement from
;; the company is
;; - you have given us amount A of money during this year
;; - we needed to spend the amount of B for our operations (fees)
;; - your overall result since start of the contract is C
;; So, after 10 years, how does the customer know that the money invested
;; on this product has been spend wisely, especially if the result is not
;; outstanding and self-speaking?

;; Example 2, this time based on some numbers:
;; You have bought the product as described above.
;; You invest 100 each month into this product.
;; The contract spans over 20 years.
;; You get a report every year.
;; After ten years, the 10th report says:
;; - we have invested 1200 last year for you (12*100)
;; - we needed to deduct 250 because of unavoidable fees
;; - after fees, the year result is 1234
;; - your total investment value is now 13578
;;
;; Is this a good a result?
;; If so, what does "good" mean in this context?
;;
;; This report addresses the latter question by simulating a comparison
;; with a fictive plain bank account.
;; It answers the question:
;; If you had taken the same amount of money to a plain bank account with
;; a fixed annual interest rate and for which no fees need to be deducted:
;; What is the interest rate to match the result as measured from the
;; financial product as described above?

;; Two models are applied:
;; - A model for a yearly result
;; - A model for a multi-year result, also called "accumulated result"

;; The Year Result Model
;; It considers the account value at start and end of one (fiscal) year.
;; The calculation is based on:
;;
;; - monthly own contributions (i.e. the money you have invested, or that
;; you have withdrawn)
;; Own contributions are assumed to occur evenly distributed at the
;; beginning of each month.
;; Meaning: Even if in reality you invest 300 each quarter in the real
;; world for your financial product, no matter if the transactions date from
;; the beginning or the end of the quarters, this model assumes that the same
;; amount has been invested per month (i.e. 100 at the beginning of each
;; month)
;; NOTE: For the first monthly contribution the full annual interest rate
;; will be applied. For the last monthly contribution only a 1/12 of the
;; annual interest will be applied.
;;
;; - total change of balance for the respective fiscal year
;; This includes all own contributions, all received interests, all deducted fees
;;
;; Example:
;; - first fiscal year
;; - at the start of the year the account balance shows a value of 0
;; - 12 monthly contributions of 100
;; - at the end of the year the account balance shows a result of 1300
;; -> this report will tell you that an annual interest rate of ~15%
;; would have been required to achieve this result.

;; The Accumulated Result Model (Multi-Year Model)
;; For this model the fiscal period does not span over just one (fiscal)
;; year but spans over several years.
;; It considers the account value at the start of the first fiscal year up
;; to and including the year of evaluation.
;; The calculation is based on:
;;
;; - yearly own contributions
;; Own contribution are assumed to occur at the beginning of each year in
;; equal chunks.
;; Meaning: Even if in reality you have invested varying amounts distributed
;; over several years into your product, this model assumes that the whole
;; amount of contributions was done at the beginning each the year with
;; the same amount, i.e. a mean value is used for the performance calculation.
;; NOTE: In contrast to the yearly result, the full annual interest
;; rate is applied to all yearly contributions.
;;
;; - total change of balance from start of the first year up to and
;; including the year of evaluation
;;
;; Example A:
;; - one fiscal year
;; - at the start of the year the account balance shows a value of 0
;; - yearly contributions of 1200 done at the beginning of the year
;; - at the end of the year the account balance shows a result of 1300
;; -> this report will tell you that an annual interest rate of ~8%
;; would have been required to achieve this result. (Note the difference
;; compared to the example from the Yearly Result Model)
;;
;; Example B:
;; - three fiscal years
;; - at the start of the year the account balance shows a value of 0
;; - 3 yearly contributions of 1200
;; - at the end of
;; -- the 1st year the account balance shows a result of 1300
;; -- the 2nd year the account balance shows a result of 2600
;; -- the 3rd year the account balance shows a result of 3900
;; -> this report will tell you that
;; - to achieve the result of year one an annual interest of ~8%
;;   is required (same as Example A)
;; - to achieve the result of year two an annual interest of ~5.5%
;;   is required (i.e. the 5.5% applied to both years)
;; - to achieve the result of year two an annual interest of ~4%
;;   is required (i.e. the ~4% applied to all three years)

;; What do I need to do to make use of this report?
;; Nothing special.
;; Create an account of type ASSET for each of your financial products.
;; Track your investment contributions as they occur in reality.
;; Contributions need to be made from accounts of type BANK.
;; Adjust the balance to match the value as reported in yearly statement
;; from your product provider.
;; Adjustments might come from e.g. INCOME accounts, deductions for
;; fees might go into e.g. EXPENSE accounts. The account type BANK must
;; not be used for those accounts.
;; Start the report, select the ASSET account(s) and enter the start date
;; of your fiscal period (start of first year) and the end date of your
;; fiscal period (last day of the n-th year).
;; Example: Start 2007-04-01, End 2016-03-31

;; Read on to learn more about how the performance is calculated.

;; This report collects a result list of following format per element:
;; (
;;   year of evaluation
;;   balance at start of year of evaluation
;;   balance at start of evaluation period (start of evaluation period is a report option)
;;   (
;;     debit into the selected accounts within the year of evaluation
;;     change of balance within year of evaluation
;;     debit into the selected accounts since start of evaluation period until year of evaluation
;;     change of balance since start of evaluation period
;;   )
;; )
;; There will be one element for each fiscal year.

;; you can see this list as debug output by running gnucash in debug mode:
;; > gnucash --debug --log gnc.scm=debug &
;; and then grep for BNCHMKRP1 in the gnucash log file
;; > grep BNCHMKRP1 /tmp/gnucash.trace

;; The yearly performance is calculated according to following formular:
;;
;;          C_e - C_s - 12 D_m
;; I_r =  ----------------------------
;;         13/2 * D_m + C_s
;;
;; with
;;
;;       I_r = the interest for the current year (i.e. the actual performance value)
;;       C_s = the capital at start of the year (i.e. the starting balance)
;;       C_e = the capital at end of the year (i.e. the ending balance)
;;       D_m = the monthly deposit
;;
;; whereas
;;
;;       D_m = debit into the selected accounts within the year of evaluation (see above) / 12
;;
;; This formular is implemented in (define get-single-year-performance ...)

;; The accumulated performance is calculated based on following formular:
;;
;;
;;                                    i
;;                                   ---
;;                       i           \             j
;;  C_i =  C_s (I_r + 1)     +  D_y       (I_r + 1)       ;; i = 1,2,3,...
;;                                   /
;;                                   ---
;;                                   j=1
;; with
;;
;;       i   = the year counter from start of evaluation until the current year of evaluation
;;       I_r = the interest for the years 1...i (i.e. the actual performance value reached until the year of evaluation)
;;       C_s = the capital at start of the evaluation period (i.e. the starting balance at the beginning of year 1)
;;       D_y = the mean yearly deposit, calculated from the deposit done for all years
;;       C_i = the capital at the end of the evaluation period (i.e. the starting balance at the end of year i)
;;
;; whereas
;;
;;       D_y = debit into the selected accounts until the year of evaluation (see above) / number of years (i)
;;
;; As this formular cannot be resolved analytically in order to determine the value of I_r, it is approximated by
;; iteratively trying out different interest rest rates from very bad too very good until reaching an interest rate
;; that would give a better result than reached by the the selected accounts.
;;
;; This iteration is implemented in
;;
;;    (define interest-on-interest .......... )
;;    (define get-trial-kap-result .......... )
;;    (define get-multi-year-performance .... )
;;
;; The range of trial interest rates is determined by the options for the "expected result corridor".
;; This is used to avoid unnecessary processing duration (which can be in the range of minutes, depending
;; on the amount of user data) and infinite looping.

;; ------------------------------------------------------------------
;; Top-level definitions
;; ------------------------------------------------------------------

(define-module (gnucash report standard-reports asset-performance))
(use-modules (gnucash core-utils)) ; for gnc:version
(use-modules (gnucash utilities)) ;; needed for gnc:debug
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext)) ;; needed for the (N_ ) translation, changed between 2.6.4 and 2.6.14
(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/html" 0) ;for gnc-build-url

(export get-account-result-list)
(export getAccountByFullName)
(export get-performance-list)

(define report-name (N_ "Asset Performance"))
(define report-version "1.0")

(define DBG_1 "BNCHMKRP1") ;; debug level 1
(define DBG_2 "BNCHMKRP2") ;; debug level 2 for getInFlowOfType
(define DBG_3 "BNCHMKRP3") ;; debug level 3 for get-multi-year-performance

(define day-duration  86400) ;; day duration in seconds
(define year-duration (* 365 day-duration)) ;; year duration in seconds

;; ------------------------------------------------------------------
;; Define the Options for this report
;; ------------------------------------------------------------------

(define (options-generator)
  (let* (
	  (options (gnc:new-options))
	  (add-option
            (lambda (new-option)
              (gnc:register-option options new-option)
	    )
	  )
	)

        ;; ---------------------------------------------------------------------------
        ;; ACCOUNTS page
        ;; ---------------------------------------------------------------------------

        (add-option
          (gnc:make-account-list-option
             gnc:pagename-accounts                  ;; page name
             (N_ "Accounts")                        ;; option name
             "a"                                    ;; position within the page
             (N_ "This is an account list option")  ;; help text
             ;; FIXME : this used to be gnc:get-current-accounts, but
             ;; that doesn't exist any more.
             (lambda () '())
             #f
             #t
          )
        )

        ;; ---------------------------------------------------------------------------
        ;; GENERAL page
        ;; ---------------------------------------------------------------------------

	(add-option
          (gnc:make-date-option
            gnc:pagename-general           ;; page name
            (N_ "Start of Benchmark Period")
            "b"
            (N_ "Set the start of the benchmark period")
            (lambda () (cons 'relative 'start-prev-year))
            #f
           'both
           '(start-cal-year start-prev-year)
          )
        )

        (add-option
          (gnc:make-date-option
            gnc:pagename-general           ;; page name
            (N_ "End of Benchmark Period")
            "c"
            (N_ "Set the end of the benchmark period")
            (lambda () (cons 'relative 'end-prev-year))
            #f
           'both
           '(end-cal-year end-prev-year)
          )
        )

        (add-option
          (gnc:make-multichoice-option
            gnc:pagename-general           ;; page name
            (N_ "Expected Result Corridor - Upper Limit")
            "d"
            (N_ "The smaller the corridor the short the report processing time.")
            '20
            (list
              (list->vector
                (list
                  '40 ;;first
                  (N_ "40%")
                  (N_ "Sell!")
                )
              )
              (list->vector
                (list
                  '30 ;;second
                  (N_ "30%")
                  (N_ "Exceptional performance")
                )
              )
              (list->vector
                (list
                  '20 ;;third
                  (N_ "20%")
                  (N_ "Excellent performance")
                )
              )
              (list->vector
                (list
                  '10 ;;fourth
                  (N_ "10%")
                  (N_ "Very good performance")
                )
              )
            )
          )
        )

        (add-option
          (gnc:make-multichoice-option
            gnc:pagename-general           ;; page name
            (N_ "Expected Result Corridor - Lower Limit")
            "e"
            (N_ "The smaller the corridor the short the report processing time.")
            '-20
            (list
              (list->vector
                (list
                  '-10 ;;first
                  (N_ "-10%")
                  (N_ "Sad performance")
                )
              )
              (list->vector
                (list
                  '-20 ;;second
                  (N_ "-20%")
                  (N_ "Don't panic")
                )
              )
              (list->vector
                (list
                  '-30 ;;third
                  (N_ "-30%")
                  (N_ "Unbelievable low performance")
                )
              )
              (list->vector
                (list
                  '-40 ;;fourth
                  (N_ "-40%")
                  (N_ "Panic!")
                )
              )
              (list->vector
                (list
                  '-80 ;;fourth
                  (N_ "-80%")
                  (N_ "???")
                )
              )
            )
          )
        )

        ;; ---------------------------------------------------------------------------
        ;; DISPLAY page
        ;; ---------------------------------------------------------------------------

        (add-option
          (gnc:make-simple-boolean-option
            gnc:pagename-display           ;; page name
            (N_ "Show Result Table")
            "f"
            (N_ "Display the yearly results in tabular format")
            #t
          )
        )

        (add-option
          (gnc:make-simple-boolean-option
            gnc:pagename-display           ;; page name
            (N_ "Show Result Graph")
            "g"
            (N_ "Display the yearly results in a graph over time")
            #t
          )
        )

        (add-option
          (gnc:make-simple-boolean-option
            gnc:pagename-display           ;; page name
            (N_ "Show Time Stamp")
            "h"
            (N_ "Display the time of creation and version information")
            #t
          )
        )

        ;; ---------------------------------------------------------------------------
        ;; return value
        ;; ---------------------------------------------------------------------------

	options ;; RETURN value
  )
)

;; ------------------------------------------------------------------
;; Calls to the GnuCash library
;; ------------------------------------------------------------------

(define get-account-name
  (lambda (acct)
       (xaccAccountGetName acct)
  )
)

(define get-account-description
  (lambda (acct)
       (xaccAccountGetDescription acct)
  )
)

;; --------------------------------------------------------------------------------------
;; check if an account is a list element (stolen from cash-flow.scm)
;; --------------------------------------------------------------------------------------

(define (same-account? a1 a2)
  (string=? (gncAccountGetGUID a1) (gncAccountGetGUID a2))
)

(define account-in-list?
  (lambda (account accounts)
    (cond
      ((null? accounts) #f)
      ((same-account? (car accounts) account) #t)
      (else
        (account-in-list? account (cdr accounts))
      )
    )
  )
)


;; --------------------------------------------------------------------------------------
;; get the account with a given account name
;; --------------------------------------------------------------------------------------

(define getAccountByFullName
  (lambda (accountName)

    (let* (
            ;; -----------------------------------------------
            ;; get the list of all accounts
            ;; -----------------------------------------------

            (accountList (gnc-account-get-descendants-sorted (gnc-get-current-root-account)))
	    (matchingAccount (car accountList))
          )

          (for-each
            (lambda (account)
	      (if (string=? accountName (gnc-account-get-full-name account))
	        (set! matchingAccount account)
	      )
            )
            accountList
          )
          matchingAccount ;; RETURN value
    )
  )
)


;; --------------------------------------------------------------------------------------
;; collect the money that flows into a given account
;; --------------------------------------------------------------------------------------

(define getInFlowOfType
  (lambda (account accountlist start-date end-date sourceAccType)

    (gnc:debug DBG_2
               " getInFlowOfType was called with: "
               " account: " account
               " accountlist: " accountlist
               " start-date " start-date
               " end-date " end-date
               " sourceAccType: " sourceAccType
    )

    (let* (
            ;; -----------------------------------------------
            ;; get the list of transactions for this account
            ;; -----------------------------------------------

            (splitList (xaccAccountGetSplitList account))

            ;; -----------------------------------------------
            ;; initialize the return value
            ;; -----------------------------------------------

            (flowValue 0.0)
          )

      ;; -----------------------------------------------
      ;; analyse each transaction
      ;; -----------------------------------------------

      (for-each
        (lambda (split)

	  (let* (
	          (splitValue      (xaccSplitGetValue split))
		  (splitDate       (xaccTransRetDatePosted (xaccSplitGetParent split)))
	          (accountFullName (gnc-account-get-full-name  account))
		  (corrAccFullName (xaccSplitGetCorrAccountFullName split))
		  (corrAccount     (getAccountByFullName corrAccFullName))
		  (corrAccType     (xaccAccountGetType corrAccount))
		)

            ;; ---------------------------------------------
            ;; only consider transactions within the given time frame
            ;; ---------------------------------------------

	    (if (and
		  (gnc:time64-le-date start-date splitDate)
		  (gnc:time64-ge-date end-date splitDate)
		)

              ;; ---------------------------------------------
              ;; only consider transactions from or to accounts outside of the list of selected accounts
              ;; ---------------------------------------------

              (if (not (account-in-list? corrAccount accountlist))

                ;; ---------------------------------------------
                ;; only consider transactions from accounts of given type
                ;; ---------------------------------------------

                (if (eq? corrAccType sourceAccType)

                  ;; ---------------------------------------------
                  ;; process up the transaction value
                  ;; note that both positive and negative flows need to be considered
                  ;; meaning: money flowing back to the source accounts will decrease the value of the total InFlow
                  ;; ---------------------------------------------

		  (let (
		         (splitValueNumeric (gnc-numeric-to-double splitValue))
                       )

		    (set! flowValue (+ splitValueNumeric flowValue))

                    (gnc:debug DBG_2
                               " getInFlowOfType Result: "
                               " Date " (qof-print-date splitDate)
                               " Split " splitValue
                               " Flow " flowValue
                               " Source " corrAccFullName
                               " Destination " accountFullName
                    )
		  )
                )
              )
	    )
	  )
        )
        splitList
      )
      flowValue ;; RETURN value
    )
  )
)

;; --------------------------------------------------------------------------------------
;;
;; --------------------------------------------------------------------------------------
(define getListOfYears
  (lambda (start-date end-date)

    (define listOfYears
      (lambda (current-year last-year )
        (if (eq? current-year last-year)
	  (list last-year)
	  (cons current-year (listOfYears (+ current-year 1) last-year))
        )
      )
    )

    (let*
      (
	(first-year (gnc:time64-get-year start-date))
	(last-year  (gnc:time64-get-year end-date))
      )
      (listOfYears first-year last-year)
    )
  )
)

;; --------------------------------------------------------------------------------------
;;
;; --------------------------------------------------------------------------------------
(define merge-result-lists
  (lambda (list1 list2)

    (define merge-cdrs
      (lambda (cdr1 cdr2)
        (map (lambda (x y) (+ x y)) cdr1 cdr2)
      )
    )

    (let
      (
        (merged-list (list))
      )
      (for-each
        (lambda (x y)
          (set! merged-list
            (cons
              (list
                (car x) ;; year
                (+ (car (cdr x)) (car (cdr y))) ;; balance-at-year-start
                (+ (car (cdr (cdr x))) (car (cdr (cdr y)))) ;; balance-at-overall-start
                (merge-cdrs (cadr (cdr (cdr x))) (cadr (cdr (cdr y)))) ;; result-data
              )
	      merged-list
            )
          )
        )
        list1 list2
      )
      (reverse merged-list) ;; RETURN value
    )
  )
)
;; --------------------------------------------------------------------------------------
;;
;; --------------------------------------------------------------------------------------
(define get-performance-list
  (lambda (result-list result-corridor)

    (define get-single-year-performance
      (lambda (kap-delta kap-start contribution)
        (let
          (
            (monthly-contribution (/ contribution 12))
          )
          (if
            (and
              (zero? kap-start)
              (zero? monthly-contribution)
            )
            ;; then avoid division by zero
            0.0
            ;; else
	    (/
              (round (* 10000 (/ (- kap-delta contribution) (+ (* 6.5 monthly-contribution) kap-start) )))
              100
            )
          )
        )
      )
    )

    (define interest-on-interest
      (lambda (year-counter max-year interest)
        (cond
	  (
            (eq? year-counter max-year)
            (expt (+ 1 (/ interest 100)) year-counter)
          )
	  (
            else
            (+
              (expt (+ 1 (/ interest 100)) year-counter)
              (interest-on-interest (+ year-counter 1) max-year interest)
            )
          )
        )
      )
    )

    (define get-trial-kap-result
      (lambda (kap-start contribution-per-year interest-rate num-years)
        (let* (
	        (contribution-interest-factor (interest-on-interest 1 num-years interest-rate))
	        (kap-start-interest-factor (expt (+ 1 (/ interest-rate 100)) num-years))
              )
            (+
               (* contribution-per-year contribution-interest-factor)
               (* kap-start                kap-start-interest-factor)
            )
        )
      )
    )

    (define get-multi-year-performance
      (lambda (kap-start kap-end contribution num-years interest-rate)
        (let* (
	        (trial-kap-result (get-trial-kap-result kap-start (/ contribution num-years) interest-rate num-years))
              )
          (gnc:debug DBG_3
                     " get-multi-year-performance was called with"
                     " interest: " interest-rate
                     " Kap-Start: " kap-start
                     " Kap-End: " kap-end
                     " Contribution: " contribution
                     " Number of Years: " num-years
          ;;)
          ;;(gnc:debug DBG_3
                     " Trial Kap Result: " trial-kap-result
          )
          (if
            (and
              (zero? kap-start)
              (zero? contribution)
            )
            ;; then
            0
            ;; else
            (if (< trial-kap-result kap-end)
                (if (>= interest-rate (car result-corridor))
                  0 ;; RETURN value
                  ;; else
                  (get-multi-year-performance
                     kap-start
                     kap-end
                     contribution
                     num-years
                     ;;(+ interest-rate 0.01) -> this leads to strange rounding errors, better use:
                     (/ (round (* 100 (+ interest-rate 0.01))) 100)
                  )
                )
                ;; else
                interest-rate ;;  RETURN value
            )
          )
        )
      )
    )

    (let
        (
          (performance-list (list))
          (year-counter 0)
        )

        (for-each
          (lambda (year-result)
            (set! performance-list
              (cons
                (let* (
                       (year (car year-result)) ;; year
                       (balance-at-year-start (car (cdr year-result)))
                       (balance-at-overall-start (car (cdr (cdr year-result))))
                       (result-data (cadr (cdr (cdr year-result))))
                       (contribution-year (car result-data))
                       (balance-delta-year (car (cdr result-data)))
                       (contribution-overall (car (cdr (cdr result-data))))
                       (balance-delta-overall (cadr (cdr (cdr result-data))))
                       (num-years (- (length result-list) year-counter))
                      )
                  (list
                    year
                    (get-single-year-performance
                      balance-delta-year
                      balance-at-year-start
                      contribution-year
                    )
                    ;;(get-single-year-performance balance-at-overall-start balance-delta-overall contribution-overall)
                    (get-multi-year-performance
                      balance-at-overall-start
                      (+ balance-at-overall-start balance-delta-overall)
                      contribution-overall
                      num-years
                      (cadr result-corridor)
                    )
                  ) ;; benchmark data
                )
                performance-list
              )
            )
            (set! year-counter (+ year-counter 1))
          )
          result-list
        )
        performance-list ;; RETURN value
    )
  )
)

;; --------------------------------------------------------------------------------------
;;
;; --------------------------------------------------------------------------------------
(define get-account-result-list
  (lambda (accountlist start-date end-date)

;;    (define get-gnc-balance
;;      (lambda (acct date)
;;        (let* (
;;                (bal
;;                  (gnc:account-get-balance-at-date
;;                    acct
;;                    date
;;                    #f ;; include children?
;;                  )
;;                )
;;                (num (gnc-numeric-num bal))
;;                (denom (gnc-numeric-denom bal))
;;              )
;;              (/ num denom 1.0)
;;        )
;;      )
;;    )

    (define get-gnc-balance
      (lambda (acct date)
        (let* ()
          (gnc-numeric-to-double
            (gnc:account-get-balance-at-date
              acct
              date
              #f ;; include children?
            )
          )
        )
      )
    )

    (define sum-of-long-years-before
      (lambda (year)
        (let ( (prev-year (- year 1)))
          (if (< prev-year 1970)
            0
            ;;(if (zero? (modulo prev-year 4))
            (if (gnc:leap-year? prev-year)
              (+ 1 (sum-of-long-years-before prev-year))
              (+ 0 (sum-of-long-years-before prev-year))
            )
          )
        )
      )
    )

    (define get-start-day-in-the-year
      (lambda (year)
        (let* (
		 (start-day-fiscal-period (- (gnc:date-get-year-day (gnc-localtime start-date)) 1))
                 ;; note that in guile the year starts with day 0, for (gnc:date-get-year-day) it starts with day 1
		 (start-year-fiscal-period (gnc:date-get-year (gnc-localtime start-date)))
              )
          ;; if the start day of  the fiscal period is between 1st Jan and 28th Feb
          ;; then the start day will be the same for all fiscal years
          ;; otherwise the day must be adjusted:
          ;; (+1) in case of a leap year and the start is on 1st Mar or later in the year
          ;; (-1) if the start is on 29th Feb and the year is not a leap year

          (if (< start-day-fiscal-period 59)
            start-day-fiscal-period
            (if (and
                      (gnc:leap-year? start-year-fiscal-period)
                      (= start-day-fiscal-period 59) ;; start day is 29th Feb
                      (not (gnc:leap-year? year))
                )
              (- 1 start-day-fiscal-period)
              (if (gnc:leap-year? year)
                  (+ 1 start-day-fiscal-period)
              )
            )
          )
        )
      )
    )

    (let*
      (
        (start-fiscal-year 0)
        (end-fiscal-year 0)
	(list-of-years (getListOfYears start-date end-date)) ;; years of the fiscal period
	(passed-days-in-year-before-start-day (get-start-day-in-the-year (car list-of-years)))
        (result-list-account (list))
        (result-list-total (list))
      )

      ;; ------------------------------------------------------
      ;; process all selected accounts
      ;; ------------------------------------------------------
      (for-each
        (lambda (account)

          ;; ------------------------------------------------------
          ;; process all identified years
          ;; ------------------------------------------------------
	  (for-each
	    (lambda (year)

              ;; ------------------------------------------------------
              ;; calculate the start date of this fiscal year
              ;; ------------------------------------------------------
              (set! start-fiscal-year
                (+ (- (* (get-start-day-in-the-year year) day-duration) 3600) (* (sum-of-long-years-before year) day-duration) (* (- year 1970) year-duration))
              )

              ;; ------------------------------------------------------
              ;; calculate the end date of this fiscal year
              ;; ------------------------------------------------------
              (set! end-fiscal-year
                (+ (- (* (- (get-start-day-in-the-year (+ year 1)) 1) day-duration) 3600) (* (sum-of-long-years-before (+ year 1)) day-duration) (* (- (+ year 1) 1970) year-duration))
              )

              (gnc:debug DBG_1
                         " get-account-result-list"
                         " Fiscal Year: " year
                         " : " (qof-print-date start-fiscal-year)
                         " - " (qof-print-date end-fiscal-year)
              )

              ;; ------------------------------------------------------
              ;; add the financial result of this fiscal
              ;; year to the result list of this account
              ;; ------------------------------------------------------
	      (set! result-list-account
	        (cons
                  (list
                    year
                    (get-gnc-balance account start-fiscal-year)
                    (get-gnc-balance account start-date)
	            (list
	              (getInFlowOfType  ;; cddr element 1: inflow for current year
                        account
	                accountlist
                        start-fiscal-year
		        (if (gnc:time64-ge-date end-fiscal-year end-date)
		          end-date ;; the overall end is before the end of the fiscal year
		          end-fiscal-year ;; vice versa
                        )
                        ACCT-TYPE-BANK
                      )
                      (gnc-numeric-to-double
                        (gnc:account-get-balance-interval  ;; cddr element 2: balance for current year
                          account
                          start-fiscal-year
		          (if (gnc:time64-ge-date end-fiscal-year end-date)
		            end-date
		            (+ end-fiscal-year day-duration)
                            ;; note that gnc:account-get-balance-interval only considers transactions
                            ;; until end-date minus 1 day. So one day is added here
                          )
                          #f ;; include children?
                        )
                      )
	              (getInFlowOfType  ;; cddr element 3: inflow from start until current year
                        account
	                accountlist
                        start-date
		        (if (gnc:time64-ge-date end-fiscal-year end-date)
		          end-date ;; the overall end is before the end of the fiscal year
		          end-fiscal-year ;; vice versa
                        )
                        ACCT-TYPE-BANK
                      )
                      (gnc-numeric-to-double
                        (gnc:account-get-balance-interval  ;; cddr element 4: balance from start until current year
                          account
                          start-date
		          (if (gnc:time64-ge-date end-fiscal-year end-date)
		            end-date ;; the overall end is before the end of the fiscal year
		            (+ end-fiscal-year day-duration) ;; vice versa
                            ;; note that gnc:account-get-balance-interval only considers transactions
                            ;; until end-date minus 1 day. So one day is added here
                          )
                          #f ;; include children?
                        )
                      )
                    )
                  )
                  result-list-account
                )
              )
	    )
            ;; -------------------------------------------------
            ;; repeat for all fiscal years
            ;; -------------------------------------------------
	    list-of-years
	  )

          (gnc:debug DBG_1
                     " get-account-result-list"
                     " Account: " (gnc-account-get-full-name account)
          )
          (gnc:debug DBG_1
                     " get-account-result-list"
                     " Result List: " result-list-account
          )


          ;; ------------------------------------------------------
          ;; merge the financial result of all years for this
          ;; account into the result of all years for all accounts
          ;; ------------------------------------------------------
          (set! result-list-total
	    (if (eq? result-list-total '())
	      result-list-account
	      (merge-result-lists result-list-account result-list-total)
            )
          )
          ;; ------------------------------------------------------
          ;; prepare for next loop
          ;; ------------------------------------------------------
	  (set! result-list-account '())
        )
        ;; -------------------------------------------------
        ;; repeat for all selected accounts
        ;; -------------------------------------------------
        accountlist
      )

      (gnc:debug DBG_1
                 " get-account-result-list"
                 " Total Result List " result-list-total
      )

      result-list-total ;; RETURN value
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
          ;; read out other options
          ;; ------------------------------------------

          (account-list (get-option-val gnc:pagename-accounts (N_"Accounts")))
          (boolean-show-stamp (get-option-val gnc:pagename-display (N_ "Show Time Stamp")))

          ;; ------------------------------------------
          ;; init output parameters
          ;; ------------------------------------------

          (document (gnc:make-html-document))
        )

        ;; ------------------------------------------
        ;; start the output document
        ;; ------------------------------------------

        (gnc:html-document-set-title! document (N_ "Asset Performance Report"))

        ;; ------------------------------------------
        ;; start evaluating the options
        ;; ------------------------------------------

        (if (null? account-list)
          (gnc:html-document-add-object!
            document
            (gnc:make-html-text
              (gnc:html-markup-p (N_ "No accounts selected."))
            )
          )
          ;; else
          (let
            (
              (boolean-show-graph (get-option-val gnc:pagename-display (N_ "Show Result Graph")))
              (boolean-show-table (get-option-val gnc:pagename-display (N_ "Show Result Table")))
            )

            (if (not (or boolean-show-graph boolean-show-table))
              (gnc:html-document-add-object!
                document
                (gnc:make-html-text
                  (gnc:html-markup-p
                    (N_
                    "You have selected to not display anything. \
                     Go to the report options and enable tabular and/or graphical result display."))
                )
              )
              ;; else
              (let*
                (
                  (start-date-val
                    (gnc:date-option-absolute-time
                      (get-option-val gnc:pagename-general (N_ "Start of Benchmark Period"))
                    )
                  )
                  (end-date-val
                    (gnc:date-option-absolute-time
                      (get-option-val gnc:pagename-general (N_ "End of Benchmark Period"))
                    )
                  )
                  (result-corridor
                    (list
                      (get-option-val gnc:pagename-general (N_ "Expected Result Corridor - Upper Limit"))
                      (get-option-val gnc:pagename-general (N_ "Expected Result Corridor - Lower Limit"))
                    )
                  )
                  (performance-list
                    (get-performance-list  ;; run the report algorithm
                      (get-account-result-list
                        account-list
                        start-date-val
                        end-date-val
                      )
                      result-corridor
                    )
                  )
                )

                ;; ------------------------------------------
                ;; list selected accounts
                ;; ------------------------------------------
                (gnc:html-document-add-object!
                  document
                  (gnc:make-html-text
                    (gnc:html-markup-p (N_ "Selected Accounts:"))
                    (gnc:html-markup-ul
                      (map
                        (lambda (acct)
                          (gnc:html-markup-anchor
                            (gnc-build-url
                              URL-TYPE-REGISTER
		              (string-append "account=" (gnc-account-get-full-name acct))
                              ""
                            )
                            (gnc-account-get-full-name acct)
                          )
                        )
                        account-list
                      )
                    )
                  )
                )

                ;; ------------------------------------------
                ;; show the fiscal period
                ;; ------------------------------------------
                (gnc:html-document-add-object!
                  document
                  (gnc:make-html-text
                    (gnc:html-markup-p
                      (_
                        (string-append
                          "Fiscal Period: "
                          (qof-print-date start-date-val )
                          " to "
                          (qof-print-date end-date-val )
	                )
                      )
                    )
                  )
                )

                ;; ------------------------------------------
                ;; show the result corridor
                ;; ------------------------------------------
                (gnc:html-document-add-object!
                  document
                  (gnc:make-html-text
                    (gnc:html-markup-p
                      (_
                        (string-append
                          "Result Corridor: "
                          (number->string (cadr result-corridor))
                          "% to "
                          (number->string (car result-corridor))
                          "%"
	                 )
                      )
                    )
                  )
                )

                ;; ------------------------------------------
                ;; show the result in tabular format
                ;; ------------------------------------------
                (if boolean-show-table
                  (let
                    (
                      (table (gnc:make-html-table))
                    )
                    (gnc:html-document-add-object!
                      document
                      (gnc:make-html-text
                        (gnc:html-markup-p (N_ "Performance Result:"))
                      )
                    )
                    (gnc:html-table-append-column!
                      table
                      (list (N_ "Year") (N_ "Single Year Performance") (N_ "Multi-Year Performance"))
                    )
                    (for-each
                      (lambda (year-result)
                        (gnc:html-table-append-column!
                          table
                          (map number->string year-result)
                        )
                      )
                     performance-list
                    )
                    (gnc:html-document-add-object! document table)
                  )
                  ;; else
                  (gnc:html-document-add-object!
                    document
                    (gnc:make-html-text
                      (gnc:html-markup-p (N_ "You have chosen not to display the result in tabular format."))
                    )
                  )
                )

                ;; ------------------------------------------
                ;; show the result graphical format
                ;; ------------------------------------------
                (if boolean-show-graph
                  (let
                    (
                      (chart (gnc:make-html-linechart))
                      (row-label-list (list))
                      (yearly-result-list (list))
                      (accumulated-result-list (list))
                    )

                    (gnc:html-linechart-set-title!        chart (N_ "Performance Graph")) ;; optional
                    ;;(gnc:html-linechart-set-subtitle!     chart (N_ "Benchmark Result"))  ;; optional
                    (gnc:html-linechart-set-y-axis-label! chart (N_ "Interest (%)"))	  ;; optional
                    (gnc:html-linechart-set-x-axis-label! chart (N_ "Fiscal Years"))      ;; optional
                    (gnc:html-linechart-set-col-labels!   chart (list (N_ "Single Year Performance") (N_ "Multi-Year Performance")))
                    (gnc:html-linechart-set-col-colors!   chart (list (N_"green") (N_ "blue")))
                    (gnc:html-linechart-set-width!        chart (cons 'pixels 600))
                    (gnc:html-linechart-set-height!       chart (cons 'pixels 400))

                    (for-each
                      (lambda (year-result)
                        (set! row-label-list
                          (cons
                            (string-append (number->string (car year-result)) "-12-31")
                            row-label-list
                          )
                        )
                        (set! yearly-result-list
                          (cons
                            (cadr year-result)
                            yearly-result-list
                          )
                        )
                        (set! accumulated-result-list
                          (cons
                            (caddr year-result)
                            accumulated-result-list
                          )
                        )
                      )
                      (reverse performance-list)
                    )

                    (gnc:html-linechart-set-row-labels! chart row-label-list)
                    (gnc:html-linechart-append-column!  chart yearly-result-list)
                    (gnc:html-linechart-append-column!  chart accumulated-result-list)

                    (gnc:html-document-add-object! document chart)
                  )
                  ;; else
                  (gnc:html-document-add-object!
                    document
                    (gnc:make-html-text
                      (gnc:html-markup-p (N_ "You have chosen not to display the result graph."))
                    )
                  )
                )
              )
            )
          )
        )

        document ;; RETURN value

  );; end let
);; end define report renderer


;; ------------------------------------------------------------------
;; Define the actual report
;; ------------------------------------------------------------------

(gnc:define-report
 'version report-version
 'name report-name
 'report-guid "def319967ab5418d90c551adc7c8138c"
 'menu-tip (N_ "Stable.")
 'menu-path (list gnc:menuname-asset-liability)
 'options-generator options-generator
 'renderer document-renderer)
