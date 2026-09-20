;;; ibuffer-tests.el --- Tests for the ibuffer settings. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Covers the ibuffer settings in init-builtin-modes.el.  So far that is
;; the Age column: `ibuffer-age-string', which turns a
;; `buffer-display-time' into "5m" / "3h" / "2d" / "1w", and the
;; `define-ibuffer-column' + `ibuffer-formats' wiring that puts it on an
;; ibuffer line.
;;
;; Run with:
;;
;;   emacs -Q --batch -l test/ibuffer-tests.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; The settings live inside a `use-package' form, which the test helper
;; stubs out, so the form's `:config' body is evaluated directly with
;; `init-test-use-package-config'.  ibuffer-vc must be installed: the
;; configured format includes its columns.
;;
;;; Code:

(require 'ert)
(add-to-list 'load-path
             (file-name-directory (or load-file-name default-directory)))
(require 'init-test-helper)

;; The configured `ibuffer-formats' has ibuffer-vc's columns in it, and
;; the nested (use-package ibuffer-vc) hits the stub, so load it by hand.
(init-test-add-elpa-package "ibuffer-vc")
(require 'ibuffer-vc)
(init-test-use-package-config "init-builtin-modes" 'ibuffer)


;;; `ibuffer-age-string'

;; A fixed NOW keeps these independent of the wall clock.
(defconst ibuffer-test--now 1000000)

(defun ibuffer-test--age (secs-ago)
  "Age string for a buffer last displayed SECS-AGO seconds before NOW."
  (ibuffer-age-string (- ibuffer-test--now secs-ago) ibuffer-test--now))

(ert-deftest ibuffer-age-never-displayed ()
  (should (equal (ibuffer-age-string nil) "")))

(ert-deftest ibuffer-age-under-a-minute ()
  (should (equal (ibuffer-test--age 0) "<1m"))
  (should (equal (ibuffer-test--age 59) "<1m")))

(ert-deftest ibuffer-age-minutes ()
  (should (equal (ibuffer-test--age 60) "1m"))
  (should (equal (ibuffer-test--age 90) "1m"))
  (should (equal (ibuffer-test--age 3599) "59m")))

(ert-deftest ibuffer-age-hours ()
  (should (equal (ibuffer-test--age 3600) "1h"))
  (should (equal (ibuffer-test--age (* 5 3600)) "5h"))
  (should (equal (ibuffer-test--age 86399) "23h")))

(ert-deftest ibuffer-age-days ()
  (should (equal (ibuffer-test--age 86400) "1d"))
  (should (equal (ibuffer-test--age 604799) "6d")))

(ert-deftest ibuffer-age-weeks ()
  (should (equal (ibuffer-test--age 604800) "1w"))
  (should (equal (ibuffer-test--age (* 3 604800)) "3w"))
  ;; No larger unit: weeks keep counting.
  (should (equal (ibuffer-test--age (* 52 604800)) "52w")))

(ert-deftest ibuffer-age-truncates-not-rounds ()
  ;; 1h59m59s is still "1h"; nothing rounds up.
  (should (equal (ibuffer-test--age (1- (* 2 3600))) "1h"))
  (should (equal (ibuffer-test--age (1- (* 2 86400))) "1d")))

(ert-deftest ibuffer-age-future-display-time ()
  ;; A display time ahead of NOW (clock stepped back) reads as fresh
  ;; rather than erroring or going negative.
  (should (equal (ibuffer-test--age -100) "<1m")))

(ert-deftest ibuffer-age-accepts-lisp-timestamps ()
  ;; `buffer-display-time' holds a Lisp timestamp, not a plain number.
  (let ((now (current-time)))
    (should (equal (ibuffer-age-string (time-subtract now 120) now) "2m"))))

(ert-deftest ibuffer-age-defaults-to-current-time ()
  (should (equal (ibuffer-age-string (time-subtract nil 7200)) "2h")))


;;; The Age column on an ibuffer line

(ert-deftest ibuffer-age-column-in-format ()
  (should (member '(age 4 -1 :right) (car ibuffer-formats))))

(defun ibuffer-test--render (display-time)
  "Return the ibuffer line for a scratch buffer last shown at DISPLAY-TIME.
The line is rendered with the configured `ibuffer-formats', so it
carries every column, not just Age."
  (let ((buf (generate-new-buffer "ibuffer-test")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq buffer-display-time display-time))
          (with-temp-buffer
            (ibuffer-mode)
            (let ((inhibit-read-only t))
              (ibuffer-insert-buffer-line buf ?\s (ibuffer-current-format)))
            (substring-no-properties (buffer-string))))
      (kill-buffer buf))))

(defun ibuffer-test--line-start (age)
  "The start of a rendered line for the scratch buffer showing AGE.
Four blank flag columns, then Name (24), Size (8) and Age (4), each
followed by a space.  Stops before Mode, which `format-mode-line'
leaves empty in batch."
  (concat "    "
          " " (format "%-24s" "ibuffer-test")
          " " (format "%8s" "0")
          " " (format "%4s" age)
          " "))

;; Age sits after the Size column, right-aligned in four columns.
(ert-deftest ibuffer-age-column-rendered ()
  (should (string-prefix-p (ibuffer-test--line-start "5m")
                           (ibuffer-test--render (time-subtract nil 300))))
  (should (string-prefix-p (ibuffer-test--line-start "<1m")
                           (ibuffer-test--render (current-time))))
  (should (string-prefix-p (ibuffer-test--line-start "3d")
                           (ibuffer-test--render
                            (time-subtract nil (* 3 86400))))))

(ert-deftest ibuffer-age-column-blank-when-never-displayed ()
  (should (string-prefix-p (ibuffer-test--line-start "")
                           (ibuffer-test--render nil))))

(provide 'ibuffer-tests)
;;; ibuffer-tests.el ends here
