;;; macsay.el --- Mac OS X's nativel tts engine.

;; Copyright (C) 2013  zxsu

;; Author: zxsu <suzp1984@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 2013-10-05 12:53 zxsu <suzp1984@gmail.com> init

;;; Code:

(defgroup say nil
  "Mac OS X's native tts engine"
  :tag "Mac OS X's TTS engine"
  :version "0.1"
  :group 'tts)

(defcustom say-program "/usr/bin/say"
  "Mac OS X's tts engine -- say"
  :type '(file :must-match t)
  :group 'say)

(provide 'macsay)
;;; macsay.el ends here
