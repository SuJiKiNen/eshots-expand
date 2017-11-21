;;; eshots-expand.el --- expand specific three characters dabbrev -*- lexical-binding: t; -*-

;; Copyright (C) 2017 SuJiKiNen

;; Author: SuJiKiNen <SuJiKiNen@gmail.com>
;; URL: https://github.com/SuJiKiNen/eshots-expand
;; Keywords: abbrev convenience
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This is a tweak of hippie expand's try-expand-dabbrev and try-expand-dabbrev-all-buffers
;; eshots coming from the typing 3 characters
;; it meaning that it only try expand when yout typed 3 characters
;; the first character and the last character are same as the expansion
;; but the middle character is arbitrary except two metioned before
;; The idea inspired from annoying choosing same prefix expansion or completion

;;; Code:

(require 'hippie-exp)

(defun eshots-expand-dabbrev (old)
  "Try to expand word \"dynamically\", searching the current buffer.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible expansions of the same
string).  It returns t if a new expansion is found, nil otherwise."
  (let ((expansion ()))
    (if (not old)
        (progn
          (he-init-string (he-dabbrev-beg) (point))
          (set-marker he-search-loc he-string-beg)
          (setq he-search-bw t)
          (setq he-tried-table (cons he-search-string he-tried-table))))

    (if (not (equal he-search-string ""))
        (save-excursion
          (save-restriction
            (if hippie-expand-no-restriction
                (widen))
            ;; Try looking backward unless inhibited.
            (if he-search-bw
                (progn
                  (goto-char he-search-loc)
                  (setq expansion (eshots-expand-dabbrev-search he-search-string t))
                  (set-marker he-search-loc (point))
                  (if (not expansion)
                      (progn
                        (set-marker he-search-loc he-string-end)
                        (setq he-search-bw ())))))

            (if (not expansion) ; Then look forward.
                (progn
                  (goto-char he-search-loc)
                  (setq expansion (eshots-expand-dabbrev-search he-search-string nil))
                  (set-marker he-search-loc (point)))))))

    (if (not expansion)
        (progn
          (if old (he-reset-string))
          ())
      (progn
        (he-substitute-string expansion t)
        t))))

(defun eshots-expand-pattern-builder (pattern)
  (let ((first-char (char-to-string (aref pattern 0)))
        (middle-char (char-to-string (aref pattern 1)))
        (last-char (char-to-string (aref pattern 2))))
    (cond ((not hippie-expand-dabbrev-as-symbol)
           (concat "\\<"
                   (regexp-quote first-char)
                   "\\w*"
                   (regexp-quote middle-char)
                   "\\w*"
                   (regexp-quote last-char)
                   "\\>"))
          (t
           (concat "\\_<"
                   (regexp-quote first-char)
                   "\\(\\sw\\|\\s_\\)*"
                   (regexp-quote middle-char)
                   "\\(\\sw\\|\\s_\\)*"
                   (regexp-quote last-char)
                   "\\_>")))))

(defun eshots-expand-dabbrev-all-buffers (old)
  "Try to expand word \"dynamically\", searching all other buffers.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible expansions of the same
string).  It returns t if a new expansion is found, nil otherwise."
  (let ((expansion ())
        (buf (current-buffer))
        (orig-case-fold-search case-fold-search))
    (if (not old)
        (progn
          (he-init-string (he-dabbrev-beg) (point))
          (setq he-search-bufs (buffer-list))
          (setq he-searched-n-bufs 0)
          (set-marker he-search-loc 1 (car he-search-bufs))))

    (if (not (equal he-search-string ""))
        (while (and he-search-bufs
                    (not expansion)
                    (or (not hippie-expand-max-buffers)
                        (< he-searched-n-bufs hippie-expand-max-buffers)))
          (set-buffer (car he-search-bufs))
          (if (and (not (eq (current-buffer) buf))
                   (if hippie-expand-only-buffers
                       (he-buffer-member hippie-expand-only-buffers)
                     (not (he-buffer-member hippie-expand-ignore-buffers))))
              (save-excursion
                (save-restriction
                  (if hippie-expand-no-restriction
                      (widen))
                  (goto-char he-search-loc)
                  (setq expansion
                        (let ((case-fold-search orig-case-fold-search))
                          (eshots-expand-dabbrev-search he-search-string nil)))
                  (set-marker he-search-loc (point))
                  (if (not expansion)
                      (progn
                        (setq he-search-bufs (cdr he-search-bufs))
                        (setq he-searched-n-bufs (1+ he-searched-n-bufs))
                        (set-marker he-search-loc 1 (car he-search-bufs))))))
            (setq he-search-bufs (cdr he-search-bufs))
            (set-marker he-search-loc 1 (car he-search-bufs)))))

    (set-buffer buf)
    (if (not expansion)
        (progn
          (if old (he-reset-string))
          ())
      (progn
        (he-substitute-string expansion t)
        t))))

(defun eshots-expand-predicate (pattern)
  (= (length pattern) 3))

(defun eshots-expand-dabbrev-search (pattern &optional reverse limit)
  (if (eshots-expand-predicate pattern)
      (let ((result ())
            (regpat (eshots-expand-pattern-builder pattern)))
        (while (and (not result)
                    (if reverse
                        (re-search-backward regpat limit t)
                      (re-search-forward regpat limit t)))
          (setq result (buffer-substring-no-properties (match-beginning 0)
                                                       (match-end 0)))
          (if (or (and hippie-expand-dabbrev-as-symbol
                       (> (match-beginning 0) (point-min))
                       (memq (char-syntax (char-after (1- (match-beginning 0))))
                             '(?_ ?w)))
                  (he-string-member result he-tried-table t))
              (setq result nil)))     ; ignore if bad prefix or already in table
        result)))

(provide 'eshots-expand)

;;; eshots-expand.el ends here
