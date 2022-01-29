;;; emacs-cookiecutter.el --- Emacs integration for cookiecutter -*- lexical-binding: t -*-

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.1
;; Homepage: https://github.com/laurencewarne/emacs-cookiecutter
;; Package-Requires: ((emacs "27.1"))


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Emacs integration for cookiecutter

;;; Code:

(require 'python)
(require 'json)
(require 'f)


(defcustom cookiecutter-template-sources
  (list (format "%s/.cookiecutters" (getenv "HOME")))
  "List of directories to search for cookiecutter templates.")

(defun cookiecutter--context (ctx-file)
  (let ((src (format "from cookiecutter.config import get_user_config
from cookiecutter.generate import generate_context
import json

config_dict = get_user_config()
ctx = generate_context(
    context_file='%s',
    default_context=config_dict['default_context'],
)
print(json.dumps(dict(ctx['cookiecutter'])))" ctx-file)))
    (json-read-from-string
     (shell-command-to-string
      (format "%s -c \"%s\"" python-shell-interpreter src)))))

(defun cookiecutter--existing-templates ()
  "Return a list of local templates."
  (mapcan #'f-directories cookiecutter-template-sources))

(defun cookiecutter ()
  (interactive)
  (let* ((templates (cookiecutter--existing-templates))
         (alist (mapcar (lambda (t) (cons (f-filename t) t)) templates))
         (template (completing-read "Template: " (mapcar #'f-filename templates)))
         (template-json (format "%s/cookiecutter.json"
                                (alist-get template alist nil nil #'string=))))
    (print (cookiecutter--context template-json))))

;; (cookiecutter--context "/home/laurencewarne/.cookiecutters/emacs-package-template/cookiecutter.json")

(provide 'emacs-cookiecutter)

;;; emacs-cookiecutter.el ends here
