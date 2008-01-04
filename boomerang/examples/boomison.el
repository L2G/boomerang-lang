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

;; main function
(defun boomison (source view lens)
  "Create a VIEW of SOURCE using LENS"
  (interactive "FSource: \nFView: \nsLens: ")
  (lexical-let* 
      ((source source)
       (view view)
       (lens lens)
       ;; setup archive
       (archive (make-temp-name source))
       ;; setup buffers
       (sourcebuf (find-file-noselect source t nil nil))
       (viewbuf (find-file-noselect view t nil nil))
       ;; setup windows
       (sourcewin (selected-window))
       (viewwin (split-window))
       ;; function to run-on-save
       (boomfun (lambda () 
                  (save-file source sourcebuf)
                  (save-file view viewbuf)
                  (run-boomerang lens archive source sourcebuf view viewbuf))))
    (progn 
      ;; setup source
      (set-window-buffer sourcewin sourcebuf)
      (set-buffer sourcebuf)
      (add-hook 'local-write-file-hooks boomfun)
      ;; setup view
      (set-window-buffer viewwin viewbuf)
      (set-buffer viewbuf)
      (add-hook 'local-write-file-hooks boomfun)
      ;; do initial sync
      (run-boomerang lens archive source sourcebuf view viewbuf))))

(defun save-file (f fbuf)
  (progn
    (save-window-excursion
      (switch-to-buffer fbuf)
      (write-region (point-min) (point-max) f nil t nil))))

(defun refresh-file (f fbuf)
  "Refresh FILENAME"
  (save-window-excursion
    (set-buffer fbuf) 
    (erase-buffer)
    (insert-file-contents f)
    (set-buffer-modified-p nil)))

(defun run-boomerang (lens archive source sourcebuf view viewbuf)
  (lexical-let* 
      ((lens lens)
       (archive archive)
       (source source)
       (sourcebuf sourcebuf)
       (view view)
       (viewbuf viewbuf)
       (boomcmd (concat "boomerang" " " "sync" " " lens " " 
                        archive " " source " " view
                        " " "-debug sync"))
       (errbuf (get-buffer-create "*boomerang output*")))
    ;; clear error buffer
    (save-window-excursion 
      (switch-to-buffer errbuf) 
      (erase-buffer))
    ;; run boomerang
    (if (eq 
        (call-process 
         shell-file-name nil 
         (list errbuf t) nil 
         shell-command-switch boomcmd)
        1)
    (progn
      (switch-to-buffer-other-window errbuf)
      (error "Boomerang produced a conflict!"))
    ;; check for errors
    ;;(if (> (buffer-size errbuf) 0) 
    ;;    (progn
    ;;      (switch-to-buffer-other-window errbuf)
    ;;      (error "Boomerang produced non-empty output!"))
    (if (not (file-exists-p view))
      ;; if no errors, check for empty view 
      (error "Boomerang failed to create %s" view)
      ;;  otherwise freshen buffers
      (progn
        ;; refresh source
        (let ((old-pnt (point)))
          (refresh-file source sourcebuf)
          (refresh-file view viewbuf)
          (goto-char old-pnt))
      (message "Created %s from %s." view source)
      )))))