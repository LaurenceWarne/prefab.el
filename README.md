# Prefab.el: Project Generation for Emacs

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

```
;; The exe is probably here but you can check using 'pipx list'
(setq prefab-cookiecutter-python-executable "~/.local/pipx/venvs/cookiecutter/bin/python3")
```

## Usage

