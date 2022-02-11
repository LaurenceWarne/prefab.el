# Prefab.el: Project Generation for Emacs

[![Min Emacs Version](https://img.shields.io/badge/Emacs-27+-7F5AB6?logo=gnu-emacs)](https://www.gnu.org/software/emacs/)

Prefab is a tool aiming to provide integration for project generation tools like [cookiecutter](https://github.com/cookiecutter/cookiecutter).

https://user-images.githubusercontent.com/17688577/152217438-13f53b7f-5d2a-44d1-a842-9a8da5e4be58.mp4

Currently only `cookiecutter` is supported though other tools such as `giter8` and `stack` templates may be implemented in future (PRs are of course welcome!).

## Installing

`cookiecutter` needs to be on your path and `prefab-cookiecutter-python-executable` needs to be set to a Python executable for which `cookiecutter` can be imported as a module.  If you did:

```bash
pip3 install cookiecutter
```

You're fine on both counts, but if you did:

```bash
pipx install cookiecutter
```

Then you will need to:

```elisp
;; The exe is probably here but you can check using 'pipx list'
(setq prefab-cookiecutter-python-executable "~/.local/pipx/venvs/cookiecutter/bin/python3")
```

Now using ![quelpa-use-package](https://github.com/quelpa/quelpa-use-package):

```lisp
(use-package prefab
  :ensure nil
  :quelpa (prefab :fetcher github :repo "LaurenceWarne/prefab.el" :stable t)
  :bind ("C-c c" . prefab)
  :config
  ;; uncomment to set the cookiecutter python executable
  ;; (setq prefab-cookiecutter-python-executable "~/.local/pipx/venvs/cookiecutter/bin/python3")
  ;; Where you want project directories to be output
  (setq prefab-cookiecutter-output-dir "~/projects"))
```

## Customisation

| Variable                                      | Description                                                                 | Default                                               |
|-----------------------------------------------|-----------------------------------------------------------------------------|-------------------------------------------------------|
| `prefab-cookiecutter-config-file`             | The cookiecutter config file location                                       | Left to cookiecutter                                  |
| `prefab-cookiecutter-output-dir`              | Where cookiecutter should output projects                                   | `(format "%s/.cookiecutter_replay/" (getenv "HOME"))` |
| `prefab-cookiecutter-python-executable`       | The path of the python executable to invoke for cookiecutter code           | `python-shell-interpreter`                            |
| `prefab-cookiecutter-get-context-from-replay` | If non-nil pre-populate the prefab transient with context from the last run | `nil`                                                 |
