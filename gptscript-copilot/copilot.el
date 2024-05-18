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

;; This is a adaption of Justine's original Emacs Copilot code
;; to use gptscript to connect to both remote and local LLMs.
;; 1. it use LLMs to complete your code or comments.
;; 2. it use gptscript to connect to LLMs and use gptscript chat state
;;    to save your edit history.
;; 3. it is language agnostic and language is determined by file extension.
;; 4. use `C-c C-k' to start completion process and 'C-g' to stop it.
;;

;;; Code:

(defgroup copilot nil
  "Large language model code completion."
  :prefix "copilot-"
  :group 'editing)

;; use gptscript to connect to LLMs
(defcustom copilot-bin
  "copilot4emacs"
  "use gptscript to connect to LLMs and complete code."
  :type 'string
  :group 'copilot)

(defcustom copilot-data-directory
  ".copilot_data"
  "local directory to save copilot data."
  :type 'string
  :group 'copilot)

;;;###autoload
(defun copilot-reset-all()
  "Clear all LLM history"
  (interactive)
  (copilot-reset "coder")     
  (copilot-reset "expert")     
  (copilot-reset "robin")     
  )

;;;###autoload
(defun copilot-reset(role)
  "Clear all LLM conversation from code completion history"
  (interactive)
  (when-let* ((curfile (let ((bfname (buffer-file-name)))
                    (if (not bfname)
                          (error "*** Please use a buffer bound with a file ***"))
                    bfname))
              (fname (file-name-base curfile))
              (copilot-data-dir (file-name-concat (file-name-directory curfile) copilot-data-directory))
              (copilot-state (file-name-concat copilot-data-dir (concat fname "." role "-state")))
              (should-stop (and (file-exists-p copilot-state) (> (file-attribute-size (file-attributes copilot-state)) 0)))
              (logfile (file-name-concat copilot-data-dir (concat fname ".copilot.log")))
              (json-object-type 'hash-table)
              (json-array-type 'list)
              (json-key-type 'string))
    (save-excursion
      ;; load/decode copilot-state json data and conversation msgs
      (set-buffer (get-buffer-create "copilot-state"))
      (goto-char (point-min))
      (insert-file-contents copilot-state)
      (goto-char (point-min))
      (let* ((data (json-read))
             (msgs (gethash "Messages" (gethash "completion" (gethash "state" (gethash "continuation" (gethash "state" data))))))
             (changed 0)
             (i 1)) ;;keep 1st system msg
        ;; clear all user-LLM conversation entries, except first 3 messages
        (while (< i (length msgs))
          (remove-nth i msgs)
          (setq changed (1+ changed))
          )
        (puthash "content" "" data)
        (puthash "result" "" (gethash "continuation" (gethash "state" data)))
        (puthash "results" nil (gethash "state" (gethash "continuation" (gethash "state" data))))

        ;; update state file content
        (when (> changed 0)
          ;; log state cleanup message
          (write-region (format "copilot(%s): reset copilot-state, code completion history clean up, update file\n" role) nil logfile 'append 'silent)
          (goto-char (point-min))
          (insert (json-encode data))
          (write-region (point-min) (point) copilot-state nil 'silent)))
      ;; release temp buffer
      (kill-buffer (current-buffer))
      )))

;;;###autoload
(defun copilot-complete (beg end &optional region)
  "When available, use selected text as prompt for code completion. When called from Lisp, use the text between BEG and END, unless the optional argument REGION is non-nil, in which case ignore BEG and END, and use the current region instead. If no text or region selected, will search from (point) backwards till reaching an empty line or head of file, use this region as prompt"
  (interactive (progn
                 (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list nil nil))))
  (copilot-do beg end "coder")
  )

;;;###autoload
(defun copilot-expert (beg end &optional region)
  "When available, use selected text as prompt for copilot expert. If no text or region selected, will search from (point) backwards till reaching an empty line or head of file, use this region as prompt"
  (interactive (progn
                 (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list nil nil))))
  (copilot-do beg end "expert")
  )

;;;###autoload
(defun copilot-robin (beg end &optional region)
  "When available, use selected text as prompt for the comedian. If no text or region selected, will search from (point) backwards till reaching an empty line or head of file, use this region as prompt"
  (interactive (progn
                 (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list nil nil))))
  (copilot-do beg end "robin")
  )

;;;###autoload
(defun copilot-do (beg end role)
  "Perform generic copilot operations"
  (interactive)
  (let* ((spot (point))
         (inhibit-quit t)
         (curfile (let ((bfname (buffer-file-name)))
                    (if (not bfname)
                          (error "*** Please use a buffer bound with a file ***"))
                    bfname))
         (fname (file-name-base curfile))
         (copilot-data-dir (file-name-concat (file-name-directory curfile) copilot-data-directory))
         (copilot-state (file-name-concat copilot-data-dir (concat fname "." role "-state")))
         (logfile (file-name-concat copilot-data-dir (concat fname ".copilot.log")))
         (lang (file-name-extension curfile))

         ;; use selected region text for code prompt if available
         ;; or capture code prompt by search backward until empty line or head of file
         (code (if (and beg end)
                     (buffer-substring-no-properties beg end)
                 (save-excursion
                   (beginning-of-line)
                   (while (progn
                            (previous-line)
                            (and (> (line-number-at-pos) 1)
                                 (not (string-match-p "^[[:space:]]*$" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))))
                   (buffer-substring-no-properties (point) spot))
                 ))

         ;; create new prompt for this interaction
         (prompt (if (string= role "coder")
                     (format
                      "[INST]Generate %s code to complete:[/INST]\n%s"
                      lang code)
                   (format
                    "[INST]Perform the following action as %s:[/INST]\n%s"
                    role code)
                   )))

    ;; remove mark for region
    (setq deactivate-mark t)

    ;; iterate text deleted within editor then purge it from copilot-state
    (when (and kill-ring
               (file-exists-p copilot-state)
               (> (file-attribute-size (file-attributes copilot-state)) 0))
      (let ((json-object-type 'hash-table)
            (json-array-type 'list)
            (json-key-type 'string))
        (save-excursion
          ;; load/decode copilot-state json data and conversation msgs
          (set-buffer (get-buffer-create "copilot-state"))
          (goto-char (point-min))
          (insert-file-contents copilot-state)
          (goto-char (point-min))
          (let* ((data (json-read))
                 (msgs (gethash "Messages" (gethash "completion" (gethash "state" (gethash "continuation" (gethash "state" data))))))
                 (changed 0) (j 0)
                 (kill-count (min 10 (length kill-ring))))
            ;; iterate over kill-ring, remove killed items from copilot-state
            (while (< j kill-count)
              (when-let* ((substring (current-kill j t))
                          (k 1) ;;skip first system msg
                          (killstr (string-trim (substring-no-properties substring))))
                (while (< k (length msgs))
                  (when-let* ((msg (elt msgs k))
                              (assistant (string= "user" (gethash "role" msg)))
                              (i (1+ k)))
                    (while (< i (length msgs))
                      (when-let* ((msg (elt msgs i))
                                  (assistant (string= "assistant" (gethash "role" msg)))
                                  (assistant-msg (gethash "text" (elt (gethash "content" msg) 0))))
                        (if-let* ((found (string-search killstr (string-trim assistant-msg))))
                            (progn 
                              ;; log state cleanup message
                              (write-region (format "copilot(%s): remove from copilot-state entries %d-%d:\n%s\n" role k i assistant-msg) nil logfile 'append 'silent)
                              (while (>= i k)
                                (remove-nth i msgs)
                                (setq i (1- i)))
                              (setq changed (1+ changed))
                              ;;exit msgs loop
                              (setq i (length msgs))
                              (setq k (1- k))
                              )
                          (setq k i)
                          (setq i (length msgs))))
                      (setq i (1+ i))
                      ))
                  (setq k (1+ k))
                  ))
              (setq j (1+ j))
              )
            
            ;; update state file content
            (when (> changed 0)
              ;; log state cleanup message
              (write-region (format "copilot(%s): copilot-state updated, update file\n" role) nil logfile 'append 'silent)
              (goto-char (point-min))
              (insert (json-encode data))
              (write-region (point-min) (point) copilot-state nil 'silent)))
          ;; release temp buffer
          (kill-buffer (current-buffer))
          )))

    ;; run copilot-bin streaming stdout into buffer catching ctrl-g
    (with-local-quit
      (call-process copilot-bin nil (list (current-buffer) nil) t
                    role
                    copilot-state
                    logfile
                    prompt))

    ;; get rid of most markdown syntax
    (let* ((end (point))
           (mdstr (concat "```" lang))
           (mdlen (length mdstr)))
      (save-excursion
        ;; cleanup raw code
        (when (string= role "coder")
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
            (setq end (- end 3))))
        ;; update kill-ring
        (when kill-ring
          (let ((new-code (string-trim (buffer-substring-no-properties spot end)))
                (i 0))
            (while (< i (length kill-ring))
              (when-let* ((killstring (elt kill-ring i))
                          (matched (string-match-p "\n.*\n" killstring))
                          (substring (string-trim (substring-no-properties killstring)))
                          (found (string-search substring new-code)))
                ;; log state cleanup message
                (write-region (format "copilot(%s): delete from kill-ring: %s\n" role substring) nil logfile 'append 'silent)
                (setq kill-ring (remove-nth i kill-ring))
                (setq i (1- i))
                )
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
(global-set-key (kbd "C-c C-x C-k") #'(lambda() (interactive) (copilot-reset "coder")))
(global-set-key (kbd "C-c C-e") 'copilot-expert)
(global-set-key (kbd "C-c C-x C-e") #'(lambda() (interactive) (copilot-reset "expert")))
(global-set-key (kbd "C-c C-y") 'copilot-robin)
(global-set-key (kbd "C-c C-x C-y") #'(lambda() (interactive) (copilot-reset "robin")))

(global-set-key (kbd "C-c C-x C-a") 'copilot-reset-all)

(provide 'copilot)

;;; ansi-mode.el ends here
