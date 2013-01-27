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

(defvar espeak-prog-args '("-v" "default")
  "espeak process args.")

(defun espeak-start ()
  "Start espeak process."
  (interactive)
  (let ((proc-name "espeak"))
    (unless (get-process proc-name)
      (setq espeak-process (eval `(start-process proc-name espeak-buffer espeak-program ,@espeak-prog-args))))))
 
(defun espeak-stop ()
  "Stop espeak process."
  (interactive)
  (when (processp espeak-process)
    (kill-process espeak-process)
    (sit-for 1))
  (setq espeak-process nil))

(defun espeakp ()
  "Return `t' if a espeak process is running, unless nil"
  (let ((espeakp (processp espeak-process)))
    (when (not espeakp)
      (espeak-start)
      (setq espeakp t))
    espeakp))

(defun espeak-say (text)
  "Say Text via espeak process"
  (interactive "sText: ")
  (when (espeakp)
    (process-send-string espeak-process (concat text "\n"))))

(provide 'espeak)
