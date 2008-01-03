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

(defun boomison (source view lens)
  "Create a view of FILENAME using L"
  (interactive "FSource: \nFView: \nsLens: ")
  (lexical-let* 
      ((archive (make-temp-name source))
       (sourcebuf (find-file-noselect source t nil nil))
       (viewbuf (find-file-noselect view t nil nil)))
    (boomison-sync nil lens archive source sourcebuf view viewbuf)))

(defun refresh (f fbuf)
  "Refresh FILENAME"
  (save-window-excursion 
    (switch-to-buffer fbuf) 
    (message "about to erase buffer")
    (erase-buffer)
    (message "erased")
    (insert-file-contents f)
    (set-buffer-modified-p nil)))

(defun boomison-sync (savefiles lens archive source sourcebuf view viewbuf)
  (lexical-let* 
      ((savefiles savefiles)
       (lens lens)
       (archive archive)
       (source source)
       (sourcebuf sourcebuf)
       (view view)
       (viewbuf viewbuf)
       (boomcmd (concat "boomerang" " " "sync" " " lens " " 
                        archive " " source " " view
                        " " "-debug sync"))
       (errbuf (get-buffer-create "*boomerang output*")))
    (message "boomcmd: %s" boomcmd)
    ;; save source and view
    (if (eq savefiles 't)
        (progn
          (save-window-excursion
            (switch-to-buffer sourcebuf)
            (write-region (point-min) (point-max) source nil t nil))
          (save-window-excursion
            (switch-to-buffer viewbuf)
            (write-region (point-min) (point-max) view nil t nil))))
    ;; clear error buffer
    (save-window-excursion 
      (switch-to-buffer errbuf) 
      (erase-buffer))
    ;; run boomerang
    (call-process
     shell-file-name
     nil
     (list errbuf t)
     nil
     shell-command-switch boomcmd)
    ;; check for errors
    ;;(if (> (buffer-size errbuf) 0) 
    ;;    (progn
    ;;      (switch-to-buffer-other-window errbuf)
    ;;      (error "Boomerang produced non-empty output!"))
    ;; if no errors, check for empty view 
    (if (not (file-exists-p view))
      (error "Boomerang did not create %s" view)
      ;;  if no errors, and view exists, freshen buffers
      (progn
        (refresh source sourcebuf)
        (switch-to-buffer sourcebuf)
        (add-hook 'local-write-file-hooks
                  (lambda () (boomison-sync t lens archive source sourcebuf view viewbuf)))
        (refresh view viewbuf)
        (switch-to-buffer-other-window viewbuf)
        (add-hook 'local-write-file-hooks
                  (lambda () (boomison-sync t lens archive source sourcebuf view viewbuf)))
      (message "Created %s from %s." view source)
      ))))
  