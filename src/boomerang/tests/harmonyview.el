;; harmonyview.el
;; provide a view of a file under a lens in emacs.
;; $Id$ 


;; import Common Lisp package
(require 'cl)

(defun create-view (f cmd)
  "Edit a view of FILENAME, using CMD to get and put"
  (interactive "FCreate view of file: \nsGET command: \nsPUT command: ")
  (lexical-let* 
      ((filename f)
       (afile  (make-temp-name (concat filename ".a")))
       (tempfile  (make-temp-name (concat filename ".tmp")))
       (getcmd (concat cmd " -c " filename " -o " afile))
       (putcmd (concat cmd " -c " filename " -a " tempfile " -o " filename))
       (errbuf (get-buffer-create "*harmony output*")))      
    (message "Creating view of %s..." filename)
    (save-window-excursion 
      (switch-to-buffer errbuf) 
      (erase-buffer))
    (call-process 
      shell-file-name
      nil
      (list errbuf t)
      nil
      shell-command-switch getcmd)
    (if (> (buffer-size errbuf) 0) 
        (progn
          (switch-to-buffer-other-window errbuf)
          (error "GET command produced non-empty output!"))
      (if (not (file-exists-p afile))
          (error "GET command (%s) did not create file %s" getcmd afile)
       (progn
         (find-file afile)
         (message "")
         (add-hook 'local-write-file-hooks 
            (lambda () (harmony-do-put filename tempfile putcmd errbuf)))
       )))))

(defun harmony-do-put (filename tempfile putcmd errbuf) 
    (write-region (point-min) (point-max) tempfile)
    (message "Translating updates back to %s..." filename)
    (save-window-excursion 
      (switch-to-buffer errbuf) 
      (erase-buffer))
    (call-process 
       shell-file-name
       nil
       (list errbuf t)
       nil
       shell-command-switch putcmd)
    (if (> (buffer-size errbuf) 0) 
        (progn
            (switch-to-buffer-other-window errbuf)
            (error "PUT failed"))
        (progn 
          (set-buffer-modified-p nil)
          (message "PUT succeeded")
          )))

(defun test ()
  (interactive)
  (let* ((default-path
          (if (string= (getenv "USER") "nate")
              (concat (getenv "HOME")
                      "/shared/harmony/writing/papers/express/impl2/")
            (concat (getenv "HOME")
                    "/e/impl2/")))
        (harmony-path 
         (read-file-name "Harmony path: " default-path default-path t))
        (harmony-options 
         (read-string 
          "Harmony options: "
          (concat "-l vcards " harmony-path "tests/address.fcl"))))
    (create-view 
     (concat harmony-path "tests/test.vcf")
     (concat harmony-path "plens "
             harmony-options))))

(defun test1 ()
  (interactive)
  (let ((harmony (if (string= (getenv "USER") "nate")
                     (concat (getenv "HOME")
                        "/shared/harmony/writing/papers/express/impl2/")
                     (concat (getenv "HOME")
                        "/e/impl2/"))))
  (create-view 
    (concat (getenv "HOME") "/Desktop/vCards-new2.vcf")
    (concat harmony "plens "
            harmony "tests/address.fcl "
            "-l vcards"))))

; (test)