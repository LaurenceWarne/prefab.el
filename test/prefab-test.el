;;; prefab-test.el --- Tests for prefab.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for prefab.el

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'f)

(require 'prefab)

(defvar prefab--cookiecutter-remote-template
  "https://github.com/LaurenceWarne/cookiecutter-eldev")

(defvar prefab--cookiecutter-test-conf-file
  (f-join default-directory ".eldev" "conf"))

(defvar prefab--test-cookiecutters-dir
  (f-join default-directory ".eldev" "cookiecutters"))

(defmacro prefab--with-user-choice (template &rest body)
  "Execute BODY and let TEMPLATE be any template prompted for (user input)."
  `(cl-letf (((symbol-function 'completing-read) (lambda (&rest _) ,template))
             (prefab-cookiecutter-output-dir
              (f-join default-directory ".eldev" "test-output-dir"))
             (prefab-cookiecutter-config-file
              prefab--cookiecutter-test-conf-file))
     (make-directory prefab--test-cookiecutters-dir t)
     (make-directory prefab-cookiecutter-output-dir t)
     (f-write-text
      (format "cookiecutters_dir: \"%s\"" prefab--test-cookiecutters-dir)
      'utf-8
      prefab--cookiecutter-test-conf-file)
     (condition-case err
         (save-current-buffer
           ,@body
           (f-delete prefab--test-cookiecutters-dir t)
           (f-delete prefab-cookiecutter-output-dir t))
       (error
        (f-delete prefab--test-cookiecutters-dir t)
        (f-delete prefab-cookiecutter-output-dir t)
        (error err)))))

(cl-defun prefab--test-args-from-transient
    (&key (template "cookiecutter-eldev")
          (project-name "my-cool-project.el")
          (project-file-name "{{ cookiecutter.project_name }}")
          (description "does cool stuff")
          (author "Me")
          (username "Myself")
          (emacs-vs "27.0"))
  "Return a set of arguments which would be returned by `prefab--ui'.

The arguments are the context attributes for cookiecutter-eldev:
TEMPLATE
PROJECT-NAME
PROJECT-FILE-NAME
DESCRIPTION
AUTHOR
USERNAME
EMACS-VS"
  (list (format "template=%s" template)
        (format "project_name=%s" project-name)
        (format "project_file_name=%s" project-file-name)
        (format "description=%s" description)
        (format "author=%s" author)
        (format "username=%s" username)
        (format "emacs_version=%s" emacs-vs)))

(describe "cookiecutter"

  (it "downloads remote template"
    (prefab--with-user-choice
     prefab--cookiecutter-remote-template
     (prefab)
     ;; https://github.com/magit/transient/issues/180
     (execute-kbd-macro (kbd "c"))
     (expect (file-exists-p
              (f-join prefab--test-cookiecutters-dir "cookiecutter-eldev")))))

  (it "creates project from template"
    (let ((project-name "project_1"))
      (prefab--with-user-choice
       prefab--cookiecutter-remote-template
       (prefab)
       (prefab--run (prefab--test-args-from-transient :project-name project-name))
       (expect (file-exists-p
                (f-join prefab-cookiecutter-output-dir project-name))))))

  (it "creates project from local template"
    (let ((project-name "project_1")
          (project-name-2 "project_2"))
      (prefab--with-user-choice
       prefab--cookiecutter-remote-template
       (prefab)
       (prefab--run (prefab--test-args-from-transient :project-name project-name))
       (prefab--run
        (prefab--test-args-from-transient :project-name project-name-2))
       (expect (file-exists-p
                (f-join prefab-cookiecutter-output-dir project-name-2)))))))
