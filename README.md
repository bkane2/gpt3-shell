GPT-3 Shell
=======

A simple package for generating text using GPT-3 from Lisp.

## Dependencies
- Quicklisp
- [ASDF version 3 or above](https://common-lisp.net/project/asdf/archives/asdf.lisp)
- [openai](https://github.com/openai/openai-python), Install with `pip install openai`.
- py4cl (loaded automatically via quicklisp)

The current version of the code has only been tested on SBCL.

## Installation
1. Install quicklisp by following instructions at https://www.quicklisp.org/beta/
2. Download the latest [asdf.lisp](https://common-lisp.net/project/asdf/#downloads) file and include it in your lisp start-up script (e.g. `.sbclrc`). I recommend also overwriting `quicklisp/asdf.lisp` to eliminate the possibility of accidentally loading the out-of-date copy of `asdf.lisp` that comes with Quicklisp be default.

## Using the package

### Initialization
Load the package in an SBCL instance and initialize with a valid OpenAI API key (and engine, optionally; the default is text-davinci-001).
```
$ sbcl
$ (ql:quickload :gpt3-shell)
$ ...[loading messages]...
$ (gpt3-shell:init <api-key> :engine "text-davinci-001")
```

### Generation
Generate a prompt using the `generate` function, given a string input. Newlines should be given as `\\n` in the Lisp string. For example:
```lisp
(gpt3-shell:generate "Complete the conversation between Alice and Bob:\\n\\nAlice:")
```

The following keyword parameters can be given as arguments to the `generate` function (see the [OpenAI](https://beta.openai.com/docs/api-reference/completions/create) documentation for more details):
* response-length
* temperature
* top-p
* frequency-penalty
* presence-penalty
* stop-seq

`stop-seq` should be a vector of strings (again with newlines as `\\n`). For example,
```lisp
(gpt3-shell:generate "Complete the conversation between Alice and Bob:\\n\\nAlice:"
  :stop-seq (vector "\\nBob:" "\\n"))
```