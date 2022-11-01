;;; Benjamin Kane 10-12-2022
;;; Functions for interfacing with OpenAI's GPT3 API.

(in-package :gpt3-shell)

(defparameter *engine* nil)
(defparameter *default-response-length* nil)
(defparameter *default-temperature* nil)
(defparameter *default-top-p* nil)
(defparameter *default-frequency-penalty* nil)
(defparameter *default-presence-penalty* nil)


(defun init (api-key &key (engine "text-davinci-001") (response-length 64)
                          (temperature 0.7) (top-p 1) (frequency-penalty 0)
                          (presence-penalty 0))
;```````````````````````````````````````````````````````````````````````````````
; Initializes the GPT3 interface with a given API key and any optional defaults.
;
  (py4cl:python-exec "import openai")
  (py4cl:python-exec "openai.api_key = " (py4cl::pythonize api-key))

  (py4cl:python-exec "def get_completion(openai, prompt, engine, max_tokens, temperature, top_p, frequency_penalty, presence_penalty, stop): return openai.Completion.create(prompt=prompt.replace('[N]', '\\n'), engine=engine, max_tokens=max_tokens, temperature=temperature, top_p=top_p, frequency_penalty=frequency_penalty, presence_penalty=presence_penalty, stop=[s.replace('[N]', '\\n') for s in stop]).choices[0][\"text\"]")

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
  (when (not stop-seq)
    (setq stop-seq (py4cl::pythonize stop-seq)))
  (let (response)
    (setq response (py4cl:python-call "get_completion" (py4cl:python-eval "openai")
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