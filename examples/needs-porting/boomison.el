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
      (;; synchronization archive
       (archive (make-temp-name source))
       ;; setup boomerang command-line invocation
       (boomcmd (concat 
                 "boomerang" " " "oldsync" " " lens " " 
                 archive " " source " " view
                 " " "-no-type-check" " " "-debug sync"))       
       ;; setup boomerang output buffer 
       (boombuf (get-buffer-create "*boomerang output*"))
       ;; run boomerang once, save exit code
       (boomexit (boom-run boombuf boomcmd))
       ;; setup source and view buffers
       (sourcebuf (find-file-noselect source t nil nil))
       (viewbuf (find-file-noselect view t nil nil))
       ;; function to run on save
       (boom-go (lambda () 
                  (progn
                    (boom-save-buffer sourcebuf)
                    (boom-save-buffer viewbuf)
                    (boom-refresh boombuf boomcmd sourcebuf viewbuf)))))
    ;; main body
    (if (eq boomexit 0) 
      (progn
        ;; cleanup windows
        (delete-other-windows)
        ;; setup boomison output
        (set-window-buffer (split-window (selected-window) (- (window-height) 5) nil) boombuf)
        ;; setup source
        (setup-win (selected-window) sourcebuf boom-go)
        ;; setup view
        (setup-win (split-window-horizontally) viewbuf boom-go))
      (progn 
        (switch-to-buffer boombuf)
        (error "Error: boomerang exited with status %d" boomexit)))))

(defun setup-win (win buf fun)
  (progn
    (set-window-buffer win buf)
    (set-buffer buf)
    (add-hook 'local-write-file-hooks fun)))

(defun boom-save-buffer (buf)
  (save-window-excursion
    (switch-to-buffer buf)
    (write-region (point-min) (point-max) (buffer-file-name) nil t nil)))

(defun boom-refresh-buffer (buf)
  (save-window-excursion
    (switch-to-buffer buf) 
    (clear-visited-file-modtime)
    (erase-buffer)
    (insert-file-contents (buffer-file-name))
    (set-buffer-modified-p nil)))

(defun boom-run (boombuf boomcmd)
  (call-process
   shell-file-name nil
   (list boombuf t) nil
   shell-command-switch boomcmd))

(defun boom-refresh (boombuf boomcmd sourcebuf viewbuf)
    ;; clear boomerang buffer
    (save-window-excursion 
      (switch-to-buffer boombuf)
      (erase-buffer))
    ;; run boomerang
    (let ((boomexit (boom-run boombuf boomcmd)))
      (if (eq boomexit 0)
          ;; if no errors, refresh buffer
          (let ((old-pnt (point)))            
            (boom-refresh-buffer sourcebuf)
            (boom-refresh-buffer viewbuf)
            (goto-char old-pnt))
      ;; otherwise, show boomerang buffer
      (progn
        (switch-to-buffer boombuf)
        (error "Error: boomerang exited with status %d" boomexit)))))
