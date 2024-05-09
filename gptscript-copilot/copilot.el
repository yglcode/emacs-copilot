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

;;;###autoload
(defun copilot-reset()
  "Clear all LLM conversation entries from history"
  (interactive)
  (when-let* ((curfile (buffer-file-name))
              (copilot-state (concat curfile ".copilot-state"))
              (should-stop (file-exists-p copilot-state))
              (logfile (concat curfile ".copilot-state.log"))
              (json-object-type 'hash-table)
              (json-array-type 'list)
              (json-key-type 'string)
              (changed 0))
    (save-excursion
      ;; load/decode copilot-state json data and conversation msgs
      (set-buffer (get-buffer-create "copilot-state"))
      (goto-char (point-min))
      (insert-file-contents copilot-state)
      (goto-char (point-min))
      (setq data (json-read))
      (setq msgs (gethash "Messages" (gethash "completion" (gethash "state" (gethash "continuation" (gethash "state" data))))))
      ;; clear all user-LLM conversation entries, except first 3 messages
      (let ((i 3))
        (while (< i (length msgs))
          (remove-nth i msgs)
          (setq changed (1+ changed))
          ))
      (puthash "content" "" data)
      (puthash "result" "" (gethash "continuation" (gethash "state" data)))

      ;; update state file content
      (when (> changed 0)
        ;; log state cleanup message
        (write-region "copilot: reset copilot-state, code completion history clean up, update file\n" nil logfile 'append 'silent)
        (goto-char (point-min))
        (insert (json-encode data))
        (write-region (point-min) (point) copilot-state nil 'silent))
      ;; release temp buffer
      (kill-buffer (current-buffer))
      )))

;;;###autoload
(defun copilot-complete (beg end &optional region)
  "When available, use selected text as prompt for code completion. When called from Lisp, use the text between BEG and END, unless the optional argument REGION is non-nil, in which case ignore BEG and END, and use the current region instead. If no text or region selected, will search from (point) backwards till reaching an empty line or head of file, use this region as prompt"
  (interactive (progn
                 (if (use-region-p)
                     (list (region-beginning) (region-end) 'region)
                   (list nil nil 'region))))
  (let* ((spot (point))
         (inhibit-quit t)
         (curfile (buffer-file-name))
         (copilot-state (concat curfile ".copilot-state"))
         (logfile (concat curfile ".copilot-state.log"))
         (lang (file-name-extension curfile))

         ;; use selected region text for code prompt if available
         ;; or capture code prompt by search backward until empty line or head of file
         (code (if (and beg end)
                   (progn
                     ;;(message "selected region: -%s-%s\n" beg end)
                     (buffer-substring-no-properties beg end)
                     )
                 (save-excursion
                   (beginning-of-line)
                   (while (progn
                            (previous-line)
                            (and (> (line-number-at-pos) 1)
                                 (not (string-match-p "^[[:space:]]*$" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))))
                   (buffer-substring-no-properties (point) spot))
                 ))

         ;; create new prompt for this interaction
         (prompt (format
                  "[INST]Generate %s code to complete:[/INST]\n%s"
                  lang code)))

    ;; remove mark for region
    (setq deactivate-mark t)

    ;; iterate text deleted within editor then purge it from copilot-state
    (when (and kill-ring (file-exists-p copilot-state))
      (let* ((json-object-type 'hash-table)
             (json-array-type 'list)
             (json-key-type 'string)
             (changed 0)
             (data nil)
             (msgs nil))
        (save-excursion
          ;; load/decode copilot-state json data and conversation msgs
          (set-buffer (get-buffer-create "copilot-state"))
          (goto-char (point-min))
          (insert-file-contents copilot-state)
          (goto-char (point-min))
          (setq data (json-read))
          (setq msgs (gethash "Messages" (gethash "completion" (gethash "state" (gethash "continuation" (gethash "state" data))))))
          ;; iterate over kill-ring, remove killed items from copilot-state
          (let ((i 0) (j 0) (killstr nil)
                (kill-count (min 10 (length kill-ring))))
            (while (< j kill-count)
              (let ((substring (current-kill j t)))
                (when (and substring
                           (string-match-p "\n.*\n" substring))
                  (setq i 0)
                  (setq killstr (string-trim (substring-no-properties substring)))
                  (while (< i (length msgs))
                    (when-let* ((msg (elt msgs i))
                                (assistant (string= "assistant" (gethash "role" msg)))
                                (assistant-msg (gethash "text" (elt (gethash "content" msg) 0)))
                                (found (string-search killstr (string-trim assistant-msg))))
                      ;; log state cleanup message
                      (write-region (format "copilot: remove from copilot-state entries %d-%d:\n%s\n" (1- i) i assistant-msg) nil logfile 'append 'silent)
                      (remove-nth i msgs)
                      (remove-nth (1- i) msgs)
                      (setq changed (1+ changed))
                      ;;exit msgs loop
                      (setq i (length msgs))
                      )
                    (setq i (1+ i))
                    )))
              (setq j (1+ j))
              )
            )
          ;; update state file content
          (when (> changed 0)
            ;; log state cleanup message
            (write-region "copilot: copilot-state updated, update file\n" nil logfile 'append 'silent)
            (goto-char (point-min))
            (insert (json-encode data))
            (write-region (point-min) (point) copilot-state nil 'silent))
          ;; release temp buffer
          (kill-buffer (current-buffer))
          )))

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
        ;; cleanup raw code
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
        ;; update kill-ring
        (when kill-ring
          (let ((new-code (string-trim (buffer-substring-no-properties spot end)))
                (i 0))
            (while (< i (length kill-ring))
              (let ((substring (elt kill-ring i)))
                (when (and substring
                           (string-match-p "\n.*\n" substring)
                           (string-search (string-trim (substring-no-properties substring)) new-code))
                  ;; log state cleanup message
                  (write-region (format "copilot: delete from kill-ring: %s\n" (string-trim (substring-no-properties substring))) nil logfile 'append 'silent)
                  (setq kill-ring (remove-nth i kill-ring))
                  (setq i (1- i))
                  ))
              (setq i (1+ i))
              )))
        ))
    
    ))

(defun remove-nth (nth list)
  (if (zerop nth) (cdr list)
    (let ((last (nthcdr (1- nth) list)))
      (setcdr last (cddr last))
      list)))


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
(global-set-key (kbd "C-c C-e") 'copilot-reset)

(provide 'copilot)

;;; ansi-mode.el ends here
