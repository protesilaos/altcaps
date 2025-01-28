;;; altcaps.el --- Apply alternating letter casing to convey sarcasm or mockery -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/altcaps
;; Version: 1.3.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Transform words to alternating letter casing in order to convey
;; sarcasm or mockery.  For example, convert this:
;;
;;     I respect the authorities
;;
;; To this:
;;
;;     i ReSpEcT tHe AuThOrItIeS
;;
;; The `altcaps' package thus makes you more effective at textual
;; communication.  Plus, you appear sophisticated.  tRuSt Me.
;;
;; Use any of the following commands to achieve the desired results:
;;
;; - `altcaps-word' :: Convert word to alternating letter casing.
;;   With optional NUM as a numeric prefix argument, operate on NUM
;;   words forward, defaulting to 1.  If NUM is negative, do so
;;   backward.  When NUM is a negative prefix without a number, it is
;;   interpreted -1.
;;
;; - `altcaps-region' :: Convert region words between BEG and END to
;;   alternating case.  BEG and END are buffer positions.  When called
;;   interactively, these are automatically determined as the active
;;   region's boundaries, else the space between `mark' and `point'.
;;
;; - `altcaps-dwim' :: Convert to alternating letter casing
;;   Do-What-I-Mean style.  With an active region, call
;;   `altcaps-region'.  Else invoke `altcaps-word' with optional NUM,
;;   per that command's functionality (read its documentation).
;;
;; The user option `altcaps-force-character-casing' forces the given
;; letter casing for specified characters.  Its value is an alist of
;; (STRING . CASE) pairs.  STRING is a single character string, while
;; CASE is the `upcase' or `downcase' symbol (code sample further
;; below).
;;
;; The idea is to always render certain characters in lower or upper
;; case, in consideration of their legibility in context.  For
;; example, the default altcaps algorithm produces this:
;;
;;     iLlIcIt IlLiBeRaL sIlLiNeSs
;;
;; Whereas if the value of this variable declares `i' to always be
;; lowercase and `L' uppercase, then we get this:
;;
;;     iLLiCiT iLLiBeRaL siLLiNeSs
;;
;; The code to do this:
;;
;;     (setq altcaps-force-character-casing
;;           '(("i" . downcase)
;;             ("l" . upcase)))
;;
;; Backronyms of ALTCAPS: Alternating Letters Transform Casual Asides
;; to Playful Statements.  ALTCAPS Lets Trolls Convert Aphorisms to
;; Proper Shitposts.

;;; Code:

(defgroup altcaps ()
  "Apply alternating letter casing to convey sarcasm or mockery.

For example, convert this:

    I respect the authorities

To this:

    i ReSpEcT tHe AuThOrItIeS

The `altcaps' package thus makes you more effective at textual
communication.  Plus, you appear more sophisticated.  tRuSt Me."
  :group 'editing)

(defcustom altcaps-force-character-casing nil
  "Force the given letter casing for specified single character strings.
This is an alist of (STRING . CASE).  STRING must satisfy
`stringp', while CASE is the symbol `upcase' or `downcase'.

The idea is to always render certain characters in lower or upper
case, in consideration of their legibility in context.  For
example, the default altcaps algorithm produces this:

    iLlIcIt IlLiBeRaL sIlLiNeSs

Whereas if the value of this variable declares `i' to always be
lowercase and `L' uppercase, then we get this (check the manual
for a code sample):

    iLLiCiT iLLiBeRaL siLLiNeSs

You do want to communicate mockery or sarcasm, though legibility
still matters!  (Regardless, I encourage everyone to use a decent
font that disambiguates characters.)"
  :type '(alist
          :key-type (string :tag "Single character string")
          :value-type (radio :tag "Letter casing"
                             (const :tag "Lower case" downcase)
                             (const :tag "Upper case" upcase)))
  :package-version '(altcaps . "1.2.0")
  :group 'altcaps)

(defun altcaps-transform (string)
  "Make STRING use alternating letter casing, ignoring blanks.
Respect the preferred casing for characters in the user option
`altcaps-force-character-casing'."
  (let ((characters (split-string (downcase string) ""))
        (casing nil)
        (processed-characters nil))
    (dolist (character characters)
      (when (string-match-p "[[:alpha:]]" character)
        (cond
         ((when-let* ((force-case (alist-get character altcaps-force-character-casing nil nil #'equal)))
            (setq character (funcall force-case character)
                  casing force-case)))
         ((eq casing 'downcase)
          (setq character (upcase character)
                casing 'upcase))
         (t
          (setq character (downcase character)
                casing 'downcase))))
        (push character processed-characters))
    (apply #'concat (nreverse processed-characters))))

(defun altcaps-replace-region (beginning end string)
  "Replace region between BEGINNING and END with STRING.
STRING is processed with `altcaps-transform'."
  (goto-char beginning)
  (delete-region beginning end)
  (insert (altcaps-transform string)))

(make-obsolete 'altcaps-replace 'altcaps-replace-region "1.2.0")

;;;###autoload
(defun altcaps-word (&optional num)
  "Convert word to alternating letter casing.

With optional NUM as a numeric prefix argument, operate on NUM
words forward, defaulting to 1.  If NUM is negative, do so
backward.

Alternating letter casing denotes sarcasm or mockery."
  (interactive "p")
  (let* ((beginning (point))
         (end (save-excursion (forward-word num) (point)))
         (original-word (buffer-substring-no-properties beginning end)))
    (unless (string-blank-p original-word)
      (altcaps-replace-region (min beginning end) (max beginning end) original-word))))

;;;###autoload
(defun altcaps-region (beg end)
  "Convert region words between BEG and END to alternating case.
BEG and END are buffer positions.  When called interactively,
these are automatically determined as the active region's
boundaries, else the space between `mark' and `point'.

Alternating letter casing denotes sarcasm or mockery."
  (interactive "r")
  (altcaps-replace-region beg end (buffer-substring-no-properties beg end)))

;;;###autoload
(defun altcaps-dwim (&optional num)
  "Convert to alternating letter casing Do-What-I-Mean style.
With an active region, call `altcaps-region'.  Else
invoke `altcaps-word' with optional NUM, per that
command's functionality (read its doc string).

Alternating letter casing denotes sarcasm or mockery."
  (interactive "p")
  (if (region-active-p)
      (altcaps-region (region-beginning) (region-end))
    (altcaps-word num)))

(provide 'altcaps)
;;; altcaps.el ends here
