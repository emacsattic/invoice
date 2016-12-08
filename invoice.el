;;; invoice.el --- interface for personal contracting invoice system

;; Copyright (C) 1995 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: extensions
;; Created: 1994-01-01
;; Last modified: 1995-06-15

;; $Id: invoice.el,v 1.5 2016/11/24 20:24:53 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; I use this to ease updating my TeX-format invoice forms for all my
;; contracts.  Those forms need my invoice-macros.tex file as well.

;;; Code:

(require 'time-stamp)

(defvar invoice-path-prefix-directory
  (expand-file-name "~/etc/corr/contract/"))

(defvar invoice-file-name-regexp "^invoice-\\([0-9]+\\)\\.tex$")

;; Don't change these without keeping invoice-insert-item in sync.
(defvar invoice-template "\n\\iitem{%s}{%d}{%d}{%%\n}")
(defvar invoice-item-start-regexp "^\\\\iitem{")
(defvar invoice-item-end-regexp "^}[ \t]*$")

(defvar invoice-time-state nil)
(defvar invoice-time-elapsed nil)
(defvar invoice-time-start nil)


(defun invoice-files-list (company)
  (let* ((dir (concat invoice-path-prefix-directory company "/"))
         (olist (file-name-all-completions "invoice-" dir))
         (nlist nil))
    (save-match-data
      (while olist
        (and (string-match invoice-file-name-regexp (car olist))
             (setq nlist (cons (car olist) nlist)))
        (setq olist (cdr olist)))
      (sort nlist (function (lambda (s1 s2)
                              (< (invoice-file-number s1)
                                 (invoice-file-number s2))))))))

(defun invoice-file-number (name)
  (save-match-data
    (and (string-match invoice-file-name-regexp name)
         (string-to-number (substring name (match-beginning 1) (match-end 1))))))

(defun invoice-find-latest-file (company)
  (let* ((flist (nreverse (invoice-files-list company)))
         (file (concat invoice-path-prefix-directory company "/" (car flist))))
    (and (file-plain-p file)
         (find-file file))))

(defun invoice-companies-alist ()
  (let* ((dir invoice-path-prefix-directory)
         (olist (file-name-all-completions "" dir))
         (useless '("./" "../"))
         (nlist nil)
         (s nil))
    (while olist
      (setq s (car olist))
      (setq olist (cdr olist))
      (and (not (member s useless))
           (file-directory-p (concat dir s))
           (setq nlist (cons (cons (substring s 0 (1- (length s))) nil)
                             nlist))))
    nlist))


(defun invoice (company)
  (interactive (list (completing-read "Invoice company: "
                                      (invoice-companies-alist)
                                      nil t)))
  (invoice-find-latest-file company)
  (goto-char (point-max)))

;; Don't modify this without keeping these variables in sync:
;; invoice-template
;; invoice-item-start-regexp
;; invoice-item-end-regexp
(defun invoice-insert-item ()
  (interactive)
  (cond (invoice-time-state
         (if (y-or-n-p "Invoice timing in progress.  Stop timing? ")
             (invoice-time-end)
           (error "Cancelling invoice item insertion; timer running."))))
  (goto-char (point-max))
  (cond ((and (re-search-backward invoice-item-start-regexp nil t)
              (re-search-forward invoice-item-end-regexp nil t))
         (newline)
         (let* ((p (point))
                (time-stamp-format "%b %02d, %y")
                (timestamp (time-stamp-string))
                (time (if invoice-time-elapsed
                          (invoice-time-elapsed invoice-time-elapsed)
                          '(0 0 0)))
                (hours (nth 0 time))
                (minutes (cond ((zerop (nth 2 time))
                                (nth 1 time))
                               (t
                                ;; round up to higher whole minute
                                (1+ (nth 1 time))))))
           (insert (format invoice-template timestamp hours minutes))
           (goto-char p)
           (forward-line 2)))))


(defun invoice-time-begin ()
  (interactive)
  (and invoice-time-state
       (error "Invoice timing is already in progress."))
  (setq invoice-time-elapsed '(0 0))
  (setq invoice-time-start (current-time))
  (setq invoice-time-state 'run)
  (and (interactive-p)
       (message "Invoice timing started %s." (current-time-string))))

(defun invoice-time-end ()
  (interactive)
  (and (null invoice-time-state)
       (error "No invoice timing currently in progress."))
  (let ((diff (invoice-time-difference (current-time) invoice-time-start)))
    (setq invoice-time-elapsed (invoice-time-sum invoice-time-elapsed diff)))
  (setq invoice-time-start nil)
  (setq invoice-time-state nil)
  (and (interactive-p)
       (message "Invoice timing ended.  Elapsed: %s"
                (invoice-time-elapsed-string invoice-time-elapsed))))

(defun invoice-time-pause ()
  (interactive)
  (cond ((eq invoice-time-state 'pause)
         (message "Invoice timing already paused."))
        ((not (eq invoice-time-state 'run))
         (error "Invoice timing not currently in progress.")))
  (let ((diff (invoice-time-difference (current-time) invoice-time-start)))
    (setq invoice-time-elapsed (invoice-time-sum invoice-time-elapsed diff)))
  (setq invoice-time-start nil)
  (setq invoice-time-state 'pause)
  (and (interactive-p)
       (message "Invoice timing paused.")))

(defun invoice-time-resume ()
  (interactive)
  (or (eq invoice-time-state 'pause)
      (error "Invoice timing not currently paused."))
  (setq invoice-time-start (current-time))
  (setq invoice-time-state 'run)
  (and (interactive-p)
       (message "Invoice timing resumed %s." (current-time-string))))

(defun invoice-time-elapsed (time)
  (let* ((min-unit 60)
         (hour-unit (* min-unit 60))
         (hours 0)
         (minutes 0)
         (seconds 0))
    (cond ((consp time)
           (setq time (invoice-time-seconds time))))
    (setq hours (/ time hour-unit))
    (setq time (- time (* hours hour-unit)))
    (setq minutes (/ time min-unit))
    (setq time (- time (* minutes min-unit)))
    (setq seconds time)
    (list hours minutes seconds)))

(defun invoice-time-elapsed-string (time)
  (apply 'format "%d:%d:%d" (invoice-time-elapsed time)))


;; Compute the difference, in seconds, of a - b, two structures
;; similar to those returned by `current-time'.
;; Use addition rather than logand since that is more robust; the low 16
;; bits of the seconds might have been incremented, making it more than 16
;; bits wide.
(defun invoice-time-difference (a b)
  (+ (lsh (- (car a) (car b)) 16)
     (- (car (cdr a)) (car (cdr b)))))

;; Return (in a new list the same in structure to that returned by
;; `current-time') the sum of the arguments.  Each argument may be a time
;; list or a single integer, a number of seconds.
;; This function keeps the high and low 16 bits of the seconds properly
;; balanced so that the lower value never exceeds 16 bits.  Otherwise, when
;; the result is passed to `current-time-string' it will toss some of the
;; "low" bits and format the time incorrectly.
(defun invoice-time-sum (&rest tmlist)
  (let ((high 0)
        (low 0)
        (micro 0)
        tem)
    (while tmlist
      (setq tem (car tmlist))
      (setq tmlist (cdr tmlist))
      (cond
       ((numberp tem)
        (setq low (+ low tem)))
       (t
        (setq high  (+ high  (or (car tem) 0)))
        (setq low   (+ low   (or (car (cdr tem)) 0)))
        (setq micro (+ micro (or (car (cdr (cdr tem))) 0))))))

    (and (>= micro 1000000)
         (progn
           (setq tem (/ micro 1000000))
           (setq low (+ low tem))
           (setq micro (- micro (* tem 1000000)))))

    (setq tem (lsh low -16))
    (and (> tem 0)
         (progn
           (setq low (logand low 65535))
           (setq high (+ high tem))))

    (list high low micro)))

;; Converts time list to number of seconds if no overflow will occur.
(defun invoice-time-seconds (time)
  (let ((high-valbits-avail (- (invoice-valbits) 16))
        (high-valbits (invoice-valbits (car time))))
    (and (< (- high-valbits-avail high-valbits) 0)
         (signal 'overflow-error
                 (list "timespec exceeds max integer size"
                       (cons 'timespec time)
                       (cons 'high-valbits-avail high-valbits-avail)
                       (cons 'high-valbits high-valbits))))
    (+ (lsh (car time) 16)
       (car (cdr time)))))

(defun invoice-valbits (&optional n)
  "Returns the number of binary bits required to represent n.
If n is not specified, this is effectively the number of valbits emacs uses
to represent ints---including the sign bit.

Negative values of n will always require VALBITS bits, the number of bits
emacs actually uses for its integer values, since the highest bit is used
for the sign; use (abs n) to ignore the sign."
  (or n (setq n -1))
  (let ((b 0))
    (while (not (zerop n))
      (setq n (lsh n -1))
      (setq b (1+ b)))
    b))

(provide 'invoice)

;;; invoice.el ends here
