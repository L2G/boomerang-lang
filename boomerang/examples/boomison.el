;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Harmony Project                                                         ;;
;; harmony@lists.seas.upenn.edu                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright (C) 2007 J. Nathan Foster and Benjamin C. Pierce                  ;;
;;                                                                             ;;
;; This library is free software; you can redistribute it and/or               ;;
;; modify it under the terms of the GNU Lesser General Public                  ;;
;; License as published by the Free Software Foundation; either                ;;
;; version 2.1 of the License, or (at your option) any later version.          ;;
;;                                                                             ;;
;; This library is distributed in the hope that it will be useful,             ;;
;; but WITHOUT ANY WARRANTY; without even the implied warranty of              ;;
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU           ;;
;; Lesser General Public License for more details.                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; /boomerang/examples/boomison.el                                             ;;
;; Boomison Emacs Mode                                                         ;;
;; $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; import Common Lisp package
(require 'cl)

(defun create-view (c a l)
  "Edit a view of FILENAME, using L"
  (interactive "FConcrete file: \nFAbstract file: \nsLens: ")
  (lexical-let* 
      ((cfile c)
       (afile a)
       (ofile  (make-temp-name cfile))
       (boomerangcmd (concat "boomerang" " " "sync" " " l " " ofile " " cfile " " afile))
       (errbuf (get-buffer-create "*boomerang output*")))
    (message "Creating view of %s..." cfile)
    (message "Boomerangcmd: %s" boomerangcmd)
    (save-window-excursion 
      (switch-to-buffer errbuf) 
      (erase-buffer))
    (call-process 
      shell-file-name
      nil
      (list errbuf t)
      nil
      shell-command-switch boomerangcmd)
    (if (> (buffer-size errbuf) 0) 
        (progn
          (switch-to-buffer-other-window errbuf)
          (error "Boomerang produced non-empty output!"))
      (if (not (file-exists-p afile))
          (error "Boomerang (%s) did not create file %s" boomerangcmd afile)
        (progn
          (add-hook 'local-write-file-hooks 
             (lambda () (message "Running Boomerang")))
          (find-file cfile)
          (find-file afile)
          (switch-to-buffer-other-window (get-buffer cfile))
          (message "Created view of %s." cfile)
       )))))

