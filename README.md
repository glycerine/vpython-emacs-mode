vpython-emacs-mode
==================

My vpython.el mode for ipython. Has *the* most essential feature: step-through
script line-by-line by pressing ctrl-n.  Anything else is just gravy.


From the top:

;;; vpython-3.92
;;; tuned specifically for ipython use.
;;; (Setq *vpython-version-string* "3.92") ; update this below when changing.
;;;
;;; vpython.el     - is a pair of modes for sending lines from a 
;;;                  script (sender) to a comint-started inferior 
;;;                  (receiver) process. We  use ctrl-n by default as 
;;;                  the "send this line and step" key. This enables 
;;;                  one to step through scripts easily when working
;;;                  with an interpreter, such as ipython, python, Pure, 
;;;                  R, Haskell, shell, and so forth. Functions for
;;;                  stepping through s-expressions for Lisp and 
;;;                  Scheme work are also available. This file is
;;;                  by default (in this version) setup for ipython.
;;;                  It should be saved as vpython.el in your ~/.emacs.d/
;;;                  directory. The ipython executable should be already
;;;                  in your path.
;;;
;;; License:      This minimal pair of (major and inferior) modes
;;;               was derived from the Emacs Octave Support modes in 
;;;               octave-mod.el and octave-inf.el, with a little help 
;;;               (and alot of inspiration) from the ess-mode.el for
;;;               R. As such it falls under the GNU General Public
;;;               License version 3 or later.
;;;
;;; Copyright (C) 2012, Author: Jason E. Aten
;;;
;;; how to install:
;;;
;;; (1) If you are changing which interpreter you want to use, examine
;;;     and adjust the "commonly-adjusted parameters" section just below.
;;;
;;; (2) Copy (this file) vpython.el into your ~/.emacs.d/  directory.
;;;
;;; (3) Then put this in your .emacs:
;;;
;;;    (load "/home/jaten/.emacs.d/vpython.el") ; adjust path to fit your home directory.
;;;    (require 'vpython-mode)
;;;    (global-set-key "\C-n" 'vpython-send-line)
;;;
;;; (4) Optionally for speed, M-x byte-compile-file <enter> 
;;;                           ~/.emacs.d/vpython.el <enter>
;;;
;;; (5) To use, do 'M-x run-vpython' to start the interpreter. Open
;;;     your script and in the script buffer do 'M-x vpython-mode'.
;;;
;;;     Or, open a file with an automatically recognized extension
;;;     (as specified below) and press 'C-n' on the first line
;;;     you want executed in the interpreter.
