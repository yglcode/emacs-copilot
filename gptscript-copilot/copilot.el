;;; copilot.el --- Emacs Copilot

;; Copyright 2023 Justine Alexandra Roberts Tunney

;; Author: Justine Tunney
;; Email: jtunney@mozilla.com
;; License: Apache 2.0
;; Version: 0.1

;; Author: Yigong Liu
;; License: Apache 2.0

;; Copyright 2023 Mozilla Foundation
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; This is a simple adaption of Justine's original Emacs Copilot code
;; to use gptscript to connect to both remote and local LLMs.
;; 1. it use LLMs to complete your code or comments.
;; 2. it use gptscript to connect to LLMs and use gptscript chat state
;;    to save your edit history.
;; 3. it is language agnostic and language is determined by file extension.
;; 4. use `C-c C-k' to start completion process and 'C-g' to stop it.
;;    currently there key bindings for c, python, go, java, you can add
;;    key bindings for your language in .emacs similar to following:
;;    (defun copilot-c-hook ()
;;      (define-key c-mode-base-map (kbd "C-c C-k") 'copilot-complete))
;;
;; To get started, try writing the first line of a function, e.g.
;;
;;     bool is_prime(int x) {
;;
;; Then place your caret at the end of the line, and press `C-c C-k` to
;; hand over control to your LLM, which should generate the rest of the
;; function implementation for you. Things are also tuned so the LLM is
;; likely to stop as soon as a function is made. Explanations and other
;; kind of ELI5 commentary is avoided too.
;;
;; Later on, if you were to write something like this:
;;
;;     int main() {
;;       for (int i = 0; i < 100;
;;
;; And ask your LLM to complete that, then your LLM will likely recall
;; that you two wrote an is_prime() function earlier, even though it's
;; only considering those two lines in the current instruction. You'll
;; most likely then see it decide to generate code to print the primes

;;; Code:

(defgroup copilot nil
  "Large language model code completion."
  :prefix "copilot-"
  :group 'editing)

;; use gptscript to connect to LLMs
(defcustom copilot-bin
  "coder4emacs"
  "use gptscript to connect to LLMs and complete code."
  :type 'string
  :group 'copilot)

;; app to clean up code completion history
(defcustom cleanup-bin
  "coder-cleanup"
  "cleanup copilot completion history."
  :type 'string
  :group 'copilot)

;;;###autoload
(defun copilot-complete ()
  (interactive)
  (let* ((spot (point))
         (inhibit-quit t)
         (curfile (buffer-file-name))
         (copilot-state (concat curfile ".copilot-state"))
         (lang (file-name-extension curfile))

         ;; extract current line, to left of caret
         ;; and the previous line, to give the llm
         (code (save-excursion
                 (dotimes (i 2)
                   (when (> (line-number-at-pos) 1)
                     (previous-line)))
                 (beginning-of-line)
                 (buffer-substring-no-properties (point) spot)))

         ;; create new prompt for this interaction
         (prompt (format
                  "[INST]Generate %s code to complete:[/INST]\n%s"
                   lang code)))

    ;; iterate text deleted within editor then purge it from copilot-state
    (when kill-ring
      (dotimes (i 10)
        (let ((substring (current-kill i t)))
          (when (and substring (string-match-p "\n.*\n" substring))
            (call-process cleanup-bin nil nil nil
                    copilot-state
                    substring)
            ))))

    ;; run copilot-bin streaming stdout into buffer catching ctrl-g
    (with-local-quit
      (call-process copilot-bin nil (list (current-buffer) nil) t
                    copilot-state
                    prompt))

    ;; get rid of most markdown syntax
    (let* ((end (point))
           (mdstr (concat "```" lang))
           (mdlen (length mdstr)))
      (save-excursion
        (goto-char spot)
        (while (search-forward "\\_" end t)
          (backward-char)
          (delete-backward-char 1 nil)
          (setq end (- end 1)))
        (goto-char spot)
        (while (search-forward mdstr end t)
          (delete-backward-char mdlen nil)
          (setq end (- end mdlen)))
        (goto-char spot)
        (while (search-forward "```" end t)
          (delete-backward-char 3 nil)
          (setq end (- end 3)))
      ))
    
    ))
  
;; define `ctrl-c ctrl-k` keybinding for llm code completion
(defun copilot-c-hook ()
  (define-key c-mode-base-map (kbd "C-c C-k") 'copilot-complete))
(add-hook 'c-mode-common-hook 'copilot-c-hook)
(defun copilot-py-hook ()
  (define-key python-mode-map (kbd "C-c C-k") 'copilot-complete))
(add-hook 'python-common-hook 'copilot-py-hook)
(defun copilot-go-hook ()
  (define-key go-mode-map (kbd "C-c C-k") 'copilot-complete))
(add-hook 'go-common-hook 'copilot-go-hook)
(defun copilot-java-hook ()
  (define-key java-mode-map (kbd "C-c C-k") 'copilot-complete))
(add-hook 'java-common-hook 'copilot-java-hook)
(global-set-key (kbd "C-c C-k") 'copilot-complete)

(provide 'copilot)

;;; ansi-mode.el ends here
