;;; tts.el --- tts mode for emacs

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author: zpcat su <suzp1984@gmail.com>
;; Version: 0.0
;; Package-Requires: (())
;; Keywords: tts

;;; Commentary:

;; This package define a major mode as a tts client in 
;; emcacs

(require 'ewoc)
(require 'festival)
(require 'espeak)
(require 'cl)

(defgroup tts nil
  "A TTS - text to speech client implemented by 
emacs lisp. It must need a backend engine."
  :tag "The emacs tts"
  :version "0.1"
  :group 'applications)

(defvar tts-default-voice nil
  "default tts voice")

(defcustom tts-voices-alist '(("festival-english-fair" . tts-voice-festival-english-fair)
                              ("festival-english-male" . tts-voice-festival-english-male)
                              ("festival-english-us-male" . tts-voice-festival-english-us-male)
                              ("espeak-en" . tts-voice-espeak-en)
                              ("espeak-zh" . tts-voice-espeak-zh-male))
  "alist of voice"
  :type '(repeat (cons string function))
  :group 'tts)

(defcustom tts-buffer "*tts*"
  "TTS buffer"
  :type 'string
  :group 'tts)

(defvar tts-espeak-engine '(("tts-start" . espeak-start)
                     ("tts-stop" . espeak-stop)
                     ("tts-say" . espeak-say))
  "espeak tts engine")

(defvar tts-festival-engine '(("tts-start" . festival-start)
                              ("tts-stop" . festival-stop)
                              ("tts-say" . festival-say))
  "festival tts engine")

;; TODO: how to find lisp store in symbal
(defvar tts-engine tts-espeak-engine
  "default tts engine, It provide an interface")

(defvar tts-mode-abbrev-table nil
  "Abbrev table used in TTS mode.")
(define-abbrev-table 'tts-mode-abbrev-table ())

(defvar tts-mode-ewoc nil
  "The ewoc object in this buffer")

(defvar tts-mode-point-insert nil
  "Position where the message being composed starts.")

(defvar tts-mode-time-format "[%H:%M:%S] "
  "Format of output of time in tts-mode.")

(defface tts-mode-default-face
  '((t (:background "RED" :weight bold)))
  "Default face")

(defun tts-voice-festival-english-fair ()
  "choice festival english fair voice"
  (interactive)
  (setq tts-engine tts-festival-engine)
  (festival-voice-english-fair))

(defun tts-voice-festival-english-male ()
  "choice festival english male voice"
  (interactive)
  (setq tts-engine tts-festival-engine)
  (festival-voice-english-male))

(defun tts-voice-festival-english-us-male ()
  "choice festival english us voice"
  (interactive)
  (setq tts-engine tts-festival-engine)
  (festival-voice-US-male))

;; TODO espeak voice choice
(defun tts-voice-espeak-en ()
  "choice espeak english voice"
  (interactive)
  (setq tts-engine tts-espeak-engine)
  (setq espeak-prog-args '("-v" "default"))
  (tts-stop))

(defun tts-voice-espeak-zh-male ()
  "choice espeak zh Mandarin voice"
  (interactive)
  (setq tts-engine tts-espeak-engine)
  (setq espeak-prog-args '("-v" "Mandarin"))
  (tts-stop))

(defun tts-voice (voice-name)
  "Interactively set the voice."
  (interactive (list (completing-read "Voice: " tts-voices-alist nil t)))
  (funcall (cdr (assoc voice-name tts-voices-alist)))
  (setq tts-default-voice voice-name))

(defun tts-ewoc-pp (body)
  "pretty print the body"
  (let ((beg (point))
        (text (plist-get body :body))
        (time (plist-get body :time)))
    ;;; make a interface here, users can choice differen voice and engine 
    ;;(funcall (cdr (assoc "tts-say" tts-engine)) text)
    (tts-say text)
    (insert (format-time-string tts-mode-time-format time))
    (insert "  ")
    (insert (propertize text 'face 'tts-mode-default-face))
    (put-text-property beg (point) 'read-only t)
    (put-text-property beg (point) 'front-sticky t)
    (put-text-property beg (point) 'rear-nonsticky t)))

(defun tts-say (text)
  "tts say something"
  (interactive "sText: ")
  (funcall (cdr (assoc "tts-say" tts-engine)) text))

(defun tts-start ()
  "start up default tts engine"
  (interactive)
  (funcall (cdr (assoc "tts-start" tts-engine))))

(defun tts-stop ()
  "stop default tts engine"
  (interactive)
  (funcall (cdr (assoc "tts-stop" tts-engine))))

(defun tts-send-current-line ()
  "Send Current to tts engine"
  (interactive)
  (when (plusp (- (point-max) tts-mode-point-insert))
    (let ((body (delete-and-extract-region tts-mode-point-insert (point-max))))
      (ewoc-enter-last tts-mode-ewoc (list :body body :time (current-time) :voice tts-default-voice)))))

(defvar tts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'tts-send-current-line)
    map)
  "TTS keymap.")

(define-derived-mode tts-mode fundamental-mode "TTS"
  "Major mode for TTS."
  (make-local-variable 'tts-mode-ewoc)
  (unless tts-mode-ewoc
    (setq tts-mode-ewoc
          (ewoc-create 'tts-ewoc-pp 
                       (substitute-command-keys "\n\\{tts-mode-map}")
                       (propertize "----" 'face `(foreground-color . "GREEN"))))
    (goto-char (point-max))
    (put-text-property (point-min) (point) 'read-only t)
    (let ((inhibit-read-only t))
      (put-text-property (point-min) (point) 'front-sticky t)
      (put-text-property (point-min) (point) 'rear-nonsticky t))
    (setq tts-mode-point-insert (point-marker)))
  (setq local-abbrev-table tts-mode-abbrev-table)
  (make-local-variable 'tts-default-voice)
  (setq tts-default-voice "espeak-en")
  (tts-voice tts-default-voice))

(defun tts (&optional buffer)
  "pop to a tts buffer"
  (interactive)
  (let* ((buf (if buffer buffer tts-buffer))
         (tts-n (if (bufferp (get-buffer buf)) 
                    nil 
                  t)))
    (message "%s" buf)
    (pop-to-buffer buf)
    (if tts-n
        (tts-mode))
    ))

(provide 'tts)
