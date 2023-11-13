;;; uk-holidays --- Summary
;;; Commentary:
;;; UK public holidays, and other UK notable dates.
;;; Code:

;; N.B. It is assumed that 1 January is defined with holiday-fixed -
;; this function only returns any extra bank holiday that is allocated
;; (if any) to compensate for New Year's Day falling on a weekend.
;;
;; Where 1 January falls on a weekend, the following Monday is a bank
;; holiday.

(require 'calendar)
(require 'holidays)

(defvar displayed-month)
(defvar displayed-year)

(defun uk-holidays-new-year-lieu-bank-holiday ()
  "Extra bank holiday if New Year's Day falls on a weekend."
  (let ((m displayed-month)
        (y displayed-year))
    (calendar-increment-month m y 1)
    (when (<= m 3)
      (let ((d (calendar-day-of-week (list 1 1 y))))
        (cond ((= d 6)
               (list (list (list 1 3 y)
                           "New Year's Day Bank Holiday")))
              ((= d 0)
               (list (list (list 1 2 y)
                           "New Year's Day Bank Holiday"))))))))

(defun uk-holidays-holiday-christmas-lieu-bank-holidays ()
  "Extra bank holidays if Christmas and/or Boxing Day fall on a weekend."
  (let ((m displayed-month)
        (y displayed-year))
    (calendar-increment-month m y -1)
    (when (>= m 10)
      (let ((d (calendar-day-of-week (list 12 25 y))))
        (cond ((= d 5)
               (list (list (list 12 28 y)
                           "Boxing Day Bank Holiday")))
              ((= d 6)
               (list (list (list 12 27 y)
                           "Boxing Day Bank Holiday")
                     (list (list 12 28 y)
                           "Christmas Day Bank Holiday")))
              ((= d 0)
               (list (list (list 12 27 y)
                           "Christmas Day Bank Holiday"))))))))

(defvar holiday-uk-holidays
  '((holiday-fixed 1 1 "New Year's Day")
    (uk-holidays-new-year-lieu-bank-holiday)
    (holiday-fixed 2 14 "Valentine's Day")
    (holiday-fixed 3 17 "St. Patrick's Day")
    (holiday-fixed 4 1 "April Fools' Day")
    (holiday-easter-etc -47 "Shrove Tuesday")
    (holiday-easter-etc -21 "Mother's Day")
    (holiday-easter-etc -2 "Good Friday")
    (holiday-easter-etc 0 "Easter Sunday")
    (holiday-easter-etc 1 "Easter Monday")
    (holiday-float 5 1 1 "Early May Bank Holiday")
    (holiday-float 5 1 -1 "Spring Bank Holiday")
    (holiday-float 6 0 3 "Father's Day")
    (holiday-float 8 1 -1 "Summer Bank Holiday")
    (holiday-fixed 10 31 "Halloween")
    (holiday-fixed 12 24 "Christmas Eve")
    (holiday-fixed 12 25 "Christmas Day")
    (holiday-fixed 12 26 "Boxing Day")
    (uk-holidays-holiday-christmas-lieu-bank-holidays)
    (holiday-fixed 12 31 "New Year's Eve")))

(provide 'uk-holidays)
;;; uk-holidays.el ends here
