# Prefab.el: Project Generation for Emacs

[![Min Emacs Version](https://img.shields.io/badge/Emacs-27+-7F5AB6?logo=gnu-emacs)](https://www.gnu.org/software/emacs/) [![MELPA](https://melpa.org/packages/prefab-badge.svg)](https://melpa.org/#/prefab)

Prefab is a tool aiming to provide integration for project generation tools like [cookiecutter](https://github.com/cookiecutter/cookiecutter).

https://user-images.githubusercontent.com/17688577/152217438-13f53b7f-5d2a-44d1-a842-9a8da5e4be58.mp4

Currently only `cookiecutter` is supported though other tools such as `giter8` and `stack` templates may be implemented in future (PRs are of course welcome!).

## Setup

`cookiecutter` needs to be on your path and `prefab-cookiecutter-python-executable` needs to be set to a Python executable for which `cookiecutter` can be imported as a module.  If you did:

```bash
pip3 install cookiecutter
```

You're fine on both counts and you can skip to [installation](#installation), but if you did:

```bash
pipx install cookiecutter
```

Then you will need to:

```elisp
;; The exe is probably here but you can check using 'pipx list'
(setq prefab-cookiecutter-python-executable "~/.local/pipx/venvs/cookiecutter/bin/python3")
```

## Installation

Now you can install it from melpa:

```lisp
(use-package prefab
  :bind ("C-c c" . prefab)
  :config
  ;; uncomment to set the cookiecutter python executable
  ;; (setq prefab-cookiecutter-python-executable "~/.local/pipx/venvs/cookiecutter/bin/python3")
  ;; Where you want project directories to be output
  (setq prefab-cookiecutter-output-dir "~/projects"))
```

## Usage

Just `M-x prefab`, from there you are prompted with a list of your local cookiecutter templates (= templates you have used previously), alternatively you can paste in a new cookiecutter url e.g. `https://github.com/audreyfeldroy/cookiecutter-pypackage`.

## Customisation

| Variable                                      | Description                                                                                                                                                                                                                                                                                           | Default                                                                                           |
|-----------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------|
| `prefab-cookiecutter-config-file`             | The cookiecutter config file location, note this is optional but can save you some boilerplate, see the cookiecutter [docs](https://cookiecutter.readthedocs.io/en/latest/advanced/user_config.html) for more information.  Only set this variable if your config file is not in `~/.cookiecutterrc`. | Left to cookiecutter (`~/.cookiecutterrc`).  Again, note existence of this file is not necessary. |
| `prefab-cookiecutter-output-dir`              | Where cookiecutter should output projects                                                                                                                                                                                                                                                             | `(format "%s/projects" (getenv "HOME"))`                                                          |
| `prefab-cookiecutter-python-executable`       | The path of the python executable to invoke for cookiecutter code                                                                                                                                                                                                                                     | `python-shell-interpreter`                                                                        |
| `prefab-cookiecutter-get-context-from-replay` | If non-nil pre-populate the prefab transient with context from the last run                                                                                                                                                                                                                           | `nil`                                                                                             |

## Contributing

`prefab.el` has been tested primarily on the most popular cookiecutter templates, please open an issue or PR if you find a template which doesn't work.
