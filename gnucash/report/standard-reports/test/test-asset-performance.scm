(use-modules (gnucash gnc-module))

(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))

(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report standard-reports asset-performance))
(use-modules (gnucash report report-system))
(use-modules (srfi srfi-64))
(use-modules (gnucash engine test srfi64-extras))

(define (run-test)
  (let (
         (passed? #f)
         (expected-result '((2001 0.0 0.0 (1200.0 1300.0 1200.0 1300.0)) (2002 1300.0 0.0 (1200.0 1300.0 2400.0 2600.0)) (2003 2600.0 0.0 (1200.0 1300.0 3600.0 3900.0)) (2001 15.38 8.34) (2002 5.13 5.46) (2003 3.08 4.06)))
       )
    (test-runner-factory gnc:test-runner)
    (test-begin "Testing/Temporary/test-asset-performance") ;; if (test-runner-factory gnc:test-runner) is commented out, this
                                                            ;; will create Testing/Temporary/test-asset-performance.log
    (test-assert "Setup Test Accounts" (test-setup))
    (test-assert "Performance Check" (equal? expected-result (test-asset-performance)))
    (set! passed? (test-passed?))
    (test-end "Testing/Temporary/test-asset-performance")
    passed?
  )
)

;; -----------------------------------------------------------------------

(define (create-branch-simple account-type account-structure)

  (define (create-account-simple account-type account-name parent-account)
    (let ((new-account (xaccMallocAccount (gnc-get-current-book))))
      (xaccAccountSetName new-account account-name)
      (xaccAccountSetType new-account account-type)
      (xaccAccountSetCommodity new-account (gnc-default-report-currency))
      (if parent-account
        (gnc-account-append-child parent-account new-account)
      )
     new-account
    )
  )

  (let* (
          (parent-name (car account-structure))
          (parent-account (create-account-simple account-type parent-name #f))
          (children (cdr account-structure))
        )
    (append
      (list (cons parent-name parent-account))
      (map
        (lambda (child-name)
          (cons child-name (create-account-simple account-type child-name parent-account))
        )
        children
      )
    )
  )
)

;; -----------------------------------------------------------------------

(define (make-yearly-tranactions env start-year number-of-years to-account from-account amount)
  (if(> number-of-years 0)
    (let (
           (date (gnc-dmy2time64-end 15 6 (+ start-year number-of-years -1))) ;; each year on 15th June
         )
      (env-create-transaction env date to-account from-account amount)
      (make-yearly-tranactions env start-year (- number-of-years 1) to-account from-account amount)
    )
  )
)

;; -----------------------------------------------------------------------

(define (test-setup)
  (let* (
          (env (create-test-env))
          (interest-account-branch-alist
            (create-branch-simple
              ACCT-TYPE-INCOME
              (list
                "Interest"  ;; parent
                "Source"    ;; child
              )
            )
          )
          (bank-account-branch-alist
            (create-branch-simple
              ACCT-TYPE-BANK
              (list
                "Bank"      ;; parent
                "Source"    ;; child
              )
            )
          )
          (test-account-branch-alist
            (create-branch-simple
              ACCT-TYPE-ASSET
              (list
                "Asset"            ;; parent
                "Contributions"    ;; child
                "GainsAndLosses"   ;; child
              )
            )
          )
          (expense-account-branch-alist
            (create-branch-simple
              ACCT-TYPE-EXPENSE
              (list
                "Expense"   ;; parent
                "Costs"      ;; child
              )
            )
          )
          (start-year-fiscal-period 2001)
          (fiscal-period-duration-number-of-years 3)
        )
    (gnc-account-append-child (gnc-get-current-root-account) (assoc-ref test-account-branch-alist "Asset"))
    (gnc-account-append-child (gnc-get-current-root-account) (assoc-ref bank-account-branch-alist "Bank"))
    (gnc-account-append-child (gnc-get-current-root-account) (assoc-ref expense-account-branch-alist "Expense"))

    (make-yearly-tranactions env
      start-year-fiscal-period fiscal-period-duration-number-of-years
      (assoc-ref test-account-branch-alist "Contributions") (assoc-ref bank-account-branch-alist "Source") 1200/1
    )

    (make-yearly-tranactions env
      start-year-fiscal-period fiscal-period-duration-number-of-years
      (assoc-ref test-account-branch-alist "GainsAndLosses") (assoc-ref interest-account-branch-alist "Source") 150/1
    )

    (make-yearly-tranactions env
      start-year-fiscal-period fiscal-period-duration-number-of-years
      (assoc-ref expense-account-branch-alist "Costs") (assoc-ref test-account-branch-alist "GainsAndLosses") 50/1
    )
    #t
  )
)

;; -----------------------------------------------------------------------

(define (test-asset-performance)
  (let* (
          (start-year-fiscal-period 2001)
          (fiscal-period-duration-number-of-years 3)
          (result-list
             (get-account-result-list
               (list (getAccountByFullName "Asset.Contributions") (getAccountByFullName "Asset.GainsAndLosses"))
               (gnc-dmy2time64-end 1 1 start-year-fiscal-period)
               (gnc-dmy2time64-end 31 12 (+ start-year-fiscal-period fiscal-period-duration-number-of-years -1))
             )
          )
          (performance-list
            (get-performance-list result-list (list '20 '0))
          )
        )
    (append (reverse result-list) performance-list)
  )
)
