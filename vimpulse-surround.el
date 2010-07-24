;;; vimpulse-surround.el --- emulates surround.vim, for vimpulse -*- coding: utf-8 -*-

;; Copyright (C) 2010 Tim Harper
;;
;; Author: Tim Harper
;; Maintainer: Tim Harper <timcharper at gmail dat com>
;;      Please send bug reports to the mailing list (see below).
;; Created: July 23 2010
;; Version: 0.1+git
;; Keywords: emulations, viper
;; Human-Keywords: vim, visual-mode, surround.vim
;; Mailing list: <implementations-list at lists.ourproject.org>
;;      Subscribe: http://tinyurl.com/implementations-list
;;      Newsgroup: nntp://news.gmane.org/gmane.emacs.vim-emulation
;;      Archives: http://dir.gmane.org/gmane.emacs.vim-emulation
;; Related: viper.el, vimpulse.el, viper-in-more-modes.el
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Vimpulse-surround emulates surround.vim, a popular plugin for vim.
;;
;; Requires a recent install of vimpulse, from git: More information
;; about vimpulse and how to install it here:

;; http://www.assembla.com/spaces/vimpulse

;; Tested with GNU Emacs 23.2

(require 'vimpulse)

(defgroup vimpulse-surround-pairs nil
  "surround.vim in Emacs"
  :prefix "vimpulse-surround-"
  :group 'vimpulse)

(defcustom vimpulse-surround-pairs
  '((")" . ("(" . ")"))
    ("(" . ("( " . " )"))
    ("]" . ("[" . "]"))
    ("[" . ("[ " . " ]"))
    ("}" . ("{" . "}"))
    ("{" . ("{ " . " }"))
    ("#" . ("#{" . "}")))
  "An alist of surround values. This only has effect on creating new surround pairs, not deleting them."
  :group 'vimpulse-surround
  :type '(repeat (cons (regexp :tag "Key")
                       (symbol :tag "Surround pair"))))

(defun vimpulse-surround-char-to-pair (char)
  (let ((pairs (assoc-default char vimpulse-surround-pairs)))
    (if pairs
        pairs
      (cons char char))))

(defvar *vimpulse-surrounding* nil
  "This value is set by the vimpulse-surround-define-text-object. It triggers vimpulse-change. Nothing to see here, move along.")

(defvar *vimpulse-surround-start-size* nil)
(defvar *vimpulse-surround-end-size* nil)

(defun vimpulse-surround-region (start end)
  "surround selection with input"
  (interactive "r")
  (message "hi")
  (let ((pair))
    (viper-special-read-and-insert-char)
    (setq pair (vimpulse-surround-char-to-pair (format "%c" (viper-char-at-pos 'backward))))
    (delete-char -1 t)
    (goto-char end)
    (insert (cdr pair))
    (goto-char start)
    (insert (car pair))
    (goto-char start)))

(defun vimpulse-surround-prepend-key-prefix (keys)
  (map 'list (lambda (key) (concat "s" key)) keys))

(defmacro vimpulse-surround-define-text-object (object args &rest body)
  (let ((forward-args '())
        (strip-keys)
        (strip-object-name (intern (concat (symbol-name object) "-strip")))
        (keys)
        (docstring (pop body)))
    (while (keywordp (car body))
      (setq keyword (pop body))
      (cond
       ((eq keyword :keys)
        (setq keys (vimpulse-surround-prepend-key-prefix (vimpulse-unquote (pop body)))))
       ((eq keyword :strip-keys)
        (setq strip-keys (vimpulse-surround-prepend-key-prefix (vimpulse-unquote (pop body)))))
       (t
        (push (pop body) forward-args)
        (push keyword forward-args))))

    (setq output '(progn))
    (when keys
      (nconc output `((vimpulse-define-text-object ,object ,args
                        ,docstring
                        ,@forward-args
                        :keys ',keys
                        (setq *vimpulse-surrounding* t)
                        ,@body))))
    (when strip-keys
      (nconc output `((vimpulse-define-text-object ,strip-object-name ,args
                        ,docstring
                        ,@forward-args
                        :keys ',strip-keys
                        (setq *vimpulse-surrounding* 'strip)
                        ,@body))))
    output))

(defun vimpulse-surround-zap-whitespace (direction boundary)
  (let ((looking_at_space? (if (= 1 direction)
                            (lambda () (looking-at "[ \t]"))
                          (lambda () (looking-back "[ \t]")))))
    (while (and (funcall looking_at_space?) (not (= (point) boundary)))
      (delete-char direction)
      (when (= 1 direction) (setq boundary (- boundary 1))))))

(defun vimpulse-surround-delete (begin end strip)
  "Deletes the surrounding characters in the range. If strip is t,
   then eliminate all surrounding whitespace around the range"
  (let ((o (make-overlay begin end)))
    (goto-char (overlay-start o)) (delete-char  1 t)
    (goto-char (overlay-end o))   (delete-char -1 t)

    (when strip
      (vimpulse-surround-zap-whitespace -1 (overlay-start o))
      (goto-char (overlay-start o))
      (vimpulse-surround-zap-whitespace 1 (overlay-end o)))
    (goto-char (overlay-start o))
    (delete-overlay o)))

(defun vimpulse-surround-change (begin end strip)
  "Delete specified surrounding items, and then prompt for a new pair."
  (let ((o (make-overlay begin end)))
    (vimpulse-surround-delete begin end strip)
    (vimpulse-surround-region (overlay-start o) (overlay-end o))
    (delete-overlay o)))

(defun vimpulse-delete-surround-or-delete (&optional BEGIN END DONT SAVE)
  "Prompts for a range. If the range returned is detected to be a surround range, dispatch to the vimpulse-surround-delete. Otherwise, dispatch to vimpulse-delete. "
  (interactive)
  (let* ((*vimpulse-surrounding* nil)
         (range (if BEGIN (list BEGIN END) (vimpulse-range))))
    (if *vimpulse-surrounding*
        (vimpulse-surround-delete (car range) (cadr range) (equal 'strip *vimpulse-surrounding*))
      (eval (append '(vimpulse-delete) range)))))

(defun vimpulse-change-surround-or-change (&optional BEGIN END DONT SAVE)
  "Prompts for a range. If the range returned is detected to be a surround range, dispatch to the vimpulse-surround-change. Otherwise, dispatch to vimpulse-change."
  (interactive)
  (let* ((*vimpulse-surrounding* nil)
         (range (if BEGIN (list BEGIN END) (vimpulse-range))))
    (if *vimpulse-surrounding*
        (vimpulse-surround-change (car range) (cadr range) (equal 'strip *vimpulse-surrounding*))
      (eval (append '(vimpulse-change) range)))))

(define-key viper-vi-basic-map "d" 'vimpulse-delete-surround-or-delete)
(define-key viper-vi-basic-map "c" 'vimpulse-change-surround-or-change)

(define-key vimpulse-visual-basic-map "s" 'vimpulse-surround-region)

(vimpulse-surround-define-text-object vimpulse-surround-paren (arg)
  "Delete surrounding parenthesis."
  :keys '("b" ")")
  :strip-keys '("(")
  (vimpulse-paren-range arg ?\( nil t))

(vimpulse-surround-define-text-object vimpulse-surround-bracket (arg)
  "Delete surrounding brackets."
  :keys '("]")
  :strip-keys '("[")
  (vimpulse-paren-range arg ?\[ nil t))

(vimpulse-surround-define-text-object vimpulse-surround-brace (arg)
  "Delete surrounding curly braces."
  :keys '("}")
  :strip-keys '("{")
  (vimpulse-paren-range arg ?\{ nil t))

(vimpulse-surround-define-text-object vimpulse-surround-angle (arg)
  "Delete surrounding curly braces."
  :keys '(">")
  :strip-keys '("<")
  (vimpulse-paren-range arg ?< nil t))

(vimpulse-surround-define-text-object vimpulse-surround-single-quote (arg)
  "Select a single quoted expression."
  :keys '("'")
  (vimpulse-quote-range arg ?' t))

(vimpulse-surround-define-text-object vimpulse-surround-double-quote (arg)
  "Select a double quoted expression."
  :keys '("\"")
  (vimpulse-quote-range arg ?\" t))

(provide 'vimpulse-surround)
