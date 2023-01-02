;;; Benjamin Kane 10-12-2022
;;; Functions for interfacing with OpenAI's GPT3 API.

(in-package :gpt3-shell)

(defparameter *engine* nil)
(defparameter *default-response-length* nil)
(defparameter *default-temperature* nil)
(defparameter *default-top-p* nil)
(defparameter *default-frequency-penalty* nil)
(defparameter *default-presence-penalty* nil)


(defun init (api-key &key (engine "text-davinci-003") (response-length 64)
                          (temperature 0.7) (top-p 1) (frequency-penalty 0)
                          (presence-penalty 0))
;```````````````````````````````````````````````````````````````````````````````
; Initializes the GPT3 interface with a given API key and any optional defaults.
;
  (py4cl:python-exec "import openai")
  (py4cl:python-exec "openai.api_key = " (py4cl::pythonize api-key))

  (defparameter *openai* (py4cl:python-eval "openai"))

  (py4cl:python-exec "def get_completion_stop(openai, prompt, engine, max_tokens, temperature, top_p, frequency_penalty, presence_penalty, stop): return openai.Completion.create(prompt=prompt.replace('[N]', '\\n'), engine=engine, max_tokens=max_tokens, temperature=temperature, top_p=top_p, frequency_penalty=frequency_penalty, presence_penalty=presence_penalty, stop=[s.replace('[N]', '\\n') for s in stop]).choices[0][\"text\"]")
  (py4cl:python-exec "def get_completion(openai, prompt, engine, max_tokens, temperature, top_p, frequency_penalty, presence_penalty, stop): return openai.Completion.create(prompt=prompt.replace('[N]', '\\n'), engine=engine, max_tokens=max_tokens, temperature=temperature, top_p=top_p, frequency_penalty=frequency_penalty, presence_penalty=presence_penalty).choices[0][\"text\"]")

  (defparameter *engine* engine)
  (defparameter *default-response-length* response-length)
  (defparameter *default-temperature* temperature)
  (defparameter *default-top-p* top-p)
  (defparameter *default-frequency-penalty* frequency-penalty)
  (defparameter *default-presence-penalty* presence-penalty)
  t
) ; END init


(defun generate (prompt &key (response-length *default-response-length*)
                             (temperature *default-temperature*)
                             (top-p *default-top-p*)
                             (frequency-penalty *default-frequency-penalty*)
                             (presence-penalty *default-presence-penalty*)
                             (stop-seq nil))
;`````````````````````````````````````````````````````````````````````````````````
; Generates a response from GPT3 given a prompt and optional parameters.
; NOTE: use [N] in the prompt for newlines.
;
  (let (response)
    (setq response (py4cl:python-call
      (if stop-seq "get_completion_stop" "get_completion")
      *openai*
      :prompt prompt
      :engine *engine*
      :max_tokens response-length
      :temperature temperature
      :top_p top-p
      :frequency_penalty frequency-penalty
      :presence_penalty presence-penalty
      :stop stop-seq))
    response
)) ; END generate


(defun generate-with-key (api-key prompt
                          &key (engine "text-davinci-003")
                               (response-length 64)
                               (temperature 0.7)
                               (top-p 1)
                               (frequency-penalty 0)
                               (presence-penalty 0)
                               (stop-seq nil))
;`````````````````````````````````````````````````````````````````````````````````
; Bypasses the initialization step by passing the API key as an argument to the generation
; function. Due to a bug where occasionally the value of *openai* becomes nil over longer
; sessions, this function is generally more reliable (but may require some overhead for
; reading and passing the api-key each time).
; NOTE: use [N] in the prompt for newlines.
;
  (when (or (not (boundp '*openai*)) (null *openai*))
    (py4cl:python-exec "import openai")
    (defparameter *openai* (py4cl:python-eval "openai")))

  (if stop-seq
    (py4cl:python-exec "def get_completion_with_key(openai, api_key, prompt, engine, max_tokens, temperature, top_p, frequency_penalty, presence_penalty, stop): return openai.Completion.create(api_key, prompt=prompt.replace('[N]', '\\n'), engine=engine, max_tokens=max_tokens, temperature=temperature, top_p=top_p, frequency_penalty=frequency_penalty, presence_penalty=presence_penalty, stop=[s.replace('[N]', '\\n') for s in stop]).choices[0][\"text\"]")
    (py4cl:python-exec "def get_completion_with_key(openai, api_key, prompt, engine, max_tokens, temperature, top_p, frequency_penalty, presence_penalty, stop): return openai.Completion.create(api_key, prompt=prompt.replace('[N]', '\\n'), engine=engine, max_tokens=max_tokens, temperature=temperature, top_p=top_p, frequency_penalty=frequency_penalty, presence_penalty=presence_penalty).choices[0][\"text\"]"))

  (let (response)
    (setq response (py4cl:python-call "get_completion_with_key" *openai*
      :api_key api-key
      :prompt prompt
      :engine engine
      :max_tokens response-length
      :temperature temperature
      :top_p top-p
      :frequency_penalty frequency-penalty
      :presence_penalty presence-penalty
      :stop stop-seq))
    response
)) ; END generate-with-key


(defun generate-safe (gen-func gen-args &key (max-tries 10))
;`````````````````````````````````````````````````````````````
; Occasionally, the OpenAI API may throw an error due to a timeout
; or overloaded servers. This function attepts to invoke a given
; generation function+args repeatedly until either a non-error, non-null
; response is given, or the number of max tries has been exceeded.
;
  (let ((i 0) response)
    (loop while (and (< i max-tries) (null response)) do
      (setq response (handler-case (apply gen-func gen-args)
        (py4cl:python-error (c) nil)))
      (incf i))
    (cond
      ((null response)
        (format t "~%===========================")
        (format t "~% There was an error attempting to generate a response from GPT-3.")
        (format t "~% Make sure that the generation function and generation arguments are both valid and in the correct format.")
        (format t "~% Otherwise, you should check that the OpenAI API is still functioning properly.")
        (format t "~%===========================~%~%")
        "")
      (t response))
)) ; END generate-safe