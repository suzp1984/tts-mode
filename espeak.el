;;; espeak.el --- emacs interface for espeak

(defgroup espeak nil
  "A TTS engine - espeak can provide 
more international language. For myself chinese."
  :tag "The espeak TTS engine"
  :version "0.1"
  :group 'tts)

(defcustom espeak-program "/usr/bin/espeak"
  "Location of the espeak program"
  :type '(file :must-match t)
  :group 'espeak)

(defcustom espeak-buffer nil
  "Buffer attached to espeak-process"
  :group 'espeak)

(defvar espeak-process nil
  "Process handle for the espeak program.")

(defvar espeak-process-alist nil
  "alist collect of processes.")

(defvar espeak-process-name "espeak-en"
  "default process name")

(defvar espeak-name-args-alist '(("espeak-en" . ("-v" "default"))
                                 ("espeak-zh" . ("-v" "Mandarin")))
  "alist for name and args")

(defvar espeak-prog-args '("-v" "default")
  "espeak process args.")

(defun espeak-start ()
  "Start espeak process."
  (interactive)
  (let ((process (get-process espeak-process-name))
        (args (cdr (assoc espeak-process-name espeak-name-args-alist)))) 
    (unless (processp process)
      (setq process (eval `(start-process espeak-process-name 
                                          espeak-buffer espeak-program 
                                          ,@args))))
    (unless (processp (cdr (assoc espeak-process-name espeak-process-alist)))
      (add-to-list 'espeak-process-alist (cons espeak-process-name  process))))
  )
 
(defun espeak-stop ()
  "Stop espeak process."
  (interactive)
  (let ((e-process (cdr (assoc espeak-process-name espeak-process-alist))))
    (when (processp e-process)
      (when (process-live-p e-process)
        (kill-process e-process)
        (sit-for 1))
      (setq espeak-process-alist 
            (assq-delete-all espeak-process-name espeak-process-alist))
      ))
  )

(defun espeakp ()
  "Return `t' if a espeak process is running, unless nil"
  (let ((e-process (cdr (assoc espeak-process-name espeak-process-alist)))
        (espeakp t))
    (when (not (and (processp e-process) (process-live-p e-process)))
      (espeak-stop)
      (espeak-start)
      (setq espeakp t))
    espeakp))

(defun espeak-say (text)
  "Say Text via espeak process"
  (interactive "sText: ")
  (when (espeakp)
    (process-send-string (cdr (assoc espeak-process-name espeak-process-alist)) (concat text "\n"))))

(provide 'espeak)
