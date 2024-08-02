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
  (file-name-concat (getenv "HOME") ".copilot4emacs_data")
  "local directory to save copilot data at top of workspace"
  :type 'string
  :group 'copilot)

(defcustom clangs
  '("c" "h" "cc" "cpp" "c++" "java" "go" "cs" "rs" "js" "ts" "css" "zig")
  "programming languages share block comments convention similar to C."
  :type 'list
  :group 'copilot)

(defcustom comments-beg-end
  '(("c" "/***" "***/") ("py" "'''" "'''") ("html" "<!--" "-->")
    ("rb" "=begin" "=end") ("lua" "--[[" "--]]"))
  "block comments conventions for languages."
  :type 'alist
  :group 'copilot)

(defcustom def-comment-beg-end
  '("``````chat" "``````")
  "fallback block comments convention."
  :type 'list
  :group 'copilot)

(defvar workspace-dir nil
  "workspace directory (top of project)")

;;;###autoload
(defun copilot-reset-history(role)
  "Clear LLM conversation from code completion history"
  (interactive (list (read-string "Clear chat history of [coder, expert, robin, all]: " "coder")))
  (cond ((member-ignore-case role '("coder" "expert" "robin"))
         (copilot-reset role))
        ((string= role "all")
         (copilot-reset "coder")     
         (copilot-reset "expert")     
         (copilot-reset "robin"))
        (t (error "*** Please choose role from (coder, expert, robin, all) ***"))
        ))
  
(defun copilot-reset(role)
  (when-let* ((curfile (let ((bfname (buffer-file-name)))
                         (if (not bfname)
                             (error "*** Please use a buffer bound with a file ***"))
                         bfname))
              (fname (file-name-base curfile))
              (copilot-data-dir (file-name-concat (proj-copilot-data-dir curfile) (data-dir-name curfile)))
              (copilot-state (file-name-concat copilot-data-dir (concat fname "." role "-state.json")))
              (should-stop (and (file-exists-p copilot-state) (> (file-attribute-size (file-attributes copilot-state)) 0)))
              (logfile (file-name-concat copilot-data-dir (concat fname ".copilot.log")))
              (json-object-type 'hash-table)
              (json-array-type 'list)
              (json-key-type 'string))
    (save-excursion
      ;; load/decode copilot-state json data and conversation msgs
      (set-buffer (get-buffer-create "copilot-state.json"))
      (goto-char (point-min))
      (insert-file-contents copilot-state)
      (goto-char (point-min))
      (let* ((data (json-read))
             (msgs (gethash "messages" (gethash "completion" (gethash "state" (gethash "continuation" (gethash "state" data))))))
             (changed 0)
             (i 1)) ;;keep 1st system msg
        ;; clear all user-LLM conversation entries, except first 3 messages
        (while (< i (length msgs))
          (remove-nth i msgs)
          (setq changed (1+ changed))
          )
        (puthash "content" "" data)
        (puthash "result" "" (gethash "continuation" (gethash "state" data)))
        ;;(puthash "results" nil (gethash "state" (gethash "continuation" (gethash "state" data))))

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
         (ffname (file-name-nondirectory curfile))
         (fname (file-name-base curfile))
         (copilot-data-dir (file-name-concat (proj-copilot-data-dir curfile) (data-dir-name curfile)))
         (copilot-state (file-name-concat copilot-data-dir (concat fname "." role "-state.json")))
         (logfile (file-name-concat copilot-data-dir (concat fname ".copilot.log")))
         (lang (downcase (file-name-extension curfile)))

         ;; use selected region text for code prompt if available
         ;; or capture code prompt by search backward until empty line or head of file
         (code (if (and beg end)
                   (buffer-substring-no-properties beg end)
                 (save-excursion
                   (beginning-of-line)
                   (if (> (line-number-at-pos) 1)
                       (while (progn
                                (previous-line)
                                (and (> (line-number-at-pos) 1)
                                     (not (string-match-p "^[[:space:]]*$" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
                                )))
                   (buffer-substring-no-properties (point) spot))
                 ))

         ;; create new prompt for this interaction
         (prompt (if (string= role "coder")
                     (format
                      ;;"[INST]Generate %s code (to add to file %s) to complete:[/INST]\n```%s\n%s"
                      ;;lang ffname lang code)
                      "[INST]Generate %s code to complete:[/INST]\n```%s\n%s"
                      lang lang code)
                   (format
                    ;;"[INST]Perform the following action as %s (result add to file %s):[/INST]\n%s"
                    ;;role ffname code)
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
          (set-buffer (get-buffer-create "copilot-state.json"))
          (goto-char (point-min))
          (insert-file-contents copilot-state)
          (goto-char (point-min))
          (let* ((data (json-read))
                 (msgs (gethash "messages" (gethash "completion" (gethash "state" (gethash "continuation" (gethash "state" data))))))
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

    (setq beg-end (get-comment-beg-end lang))
    ;; check if not a program file
    (setq non-prog (equal beg-end def-comment-beg-end))
    (if (or (not (string= role "coder")) non-prog)
        (insert (format "\n\n%s\n" (nth 0 beg-end))))
    
    ;; run copilot-bin streaming stdout into buffer catching ctrl-g
    (with-local-quit
      (call-process copilot-bin nil (list (current-buffer) nil) t
                    role
                    workspace-dir
                    copilot-state
                    logfile
                    prompt))

    (if (or (not (string= role "coder")) non-prog)
        (insert (format "\n%s\n\n" (nth 1 beg-end))))
    
    ;; get rid of most markdown syntax
    (let* ((end (point))
           (mdtail "```")
           (mdhead "```[a-zA-z]*"))
      (save-excursion
        ;; cleanup raw code
        (when (and (string= role "coder") (not non-prog))
          (goto-char spot)
          (while (search-forward "\\_" end t)
            (backward-char)
            (delete-backward-char 1 nil)
            (setq end (- end 1)))
          ;; remove heading text
          (goto-char spot)
          (when-let* ((pos (search-forward-regexp mdhead end t))
                      (hlen (- pos spot)))
            (goto-char (1+ pos)) ;;remove next space or newline
            (delete-backward-char (1+ hlen) nil)
            (setq end (- end hlen)))
          ;; remove trailing text
          (goto-char end)
          (when-let* ((pos (search-backward mdtail spot t))
                      (tlen (- end pos)))
            (goto-char end)
            (delete-backward-char tlen nil)
            (setq end (- end tlen))))
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

;; std file-name-concat added at emacs version 28.1, add a substitute here
(defun file-name-concat (dir fname)
  (concat (file-name-as-directory dir) fname))

(defun proj-copilot-data-dir(file-name)
  (file-name-concat copilot-data-directory (ws-dir-name file-name)))

(defun ws-dir(file-name)
  (progn
    (if (null workspace-dir)
        (setq workspace-dir (read-string "Workspace directory (top of project): " (file-name-directory file-name)))
      ;; move outside current workspace
      (if (string-prefix-p "../" (file-relative-name file-name workspace-dir))
          (setq workspace-dir (read-string (format "Workspace directory (current: %s): " workspace-dir) (file-name-directory file-name)))))
    workspace-dir))

(defun data-name(fname)
  (let* ((dname (combine-and-quote-strings (split-string fname "[\\\/]+") "-")))
    (if (string-prefix-p "-" dname)
        (substring dname 1)
    dname)))

(defun ws-dir-name(file-name)
  (let* ((wsdir (directory-file-name (ws-dir file-name)))
         (homedir (getenv "HOME")))
    (if (equal t (compare-strings homedir 0 (length homedir) wsdir 0 (length homedir) 'true))
        (setq wsdir (file-relative-name wsdir homedir)))
    (data-name wsdir)))

(defun data-dir-name (file-name)
  (let* ((wsdir (ws-dir file-name))
         (fname file-name))
    (if (equal t (compare-strings wsdir 0 (length wsdir) file-name 0 (length wsdir) 'true))
        (setq fname (file-relative-name file-name wsdir)))
    (data-name fname)))

(defun get-comment-beg-end(lang)
  (progn
    (if (member lang clangs)
        (setq lang "c"))
    (setq comm (cdr (assoc lang comments-beg-end)))
    (if (not comm)
        (setq comm def-comment-beg-end))
    comm))

;; define keybinding for llm code completion
(defun copilot-c-hook ()
  (define-key c-mode-base-map (kbd "C-c C-x C-k") 'copilot-complete))
(add-hook 'c-mode-common-hook 'copilot-c-hook)
(defun copilot-py-hook ()
  (define-key python-mode-map (kbd "C-c C-x C-k") 'copilot-complete))
(add-hook 'python-common-hook 'copilot-py-hook)
(defun copilot-go-hook ()
  (define-key go-mode-map (kbd "C-c C-x C-k") 'copilot-complete))
(add-hook 'go-common-hook 'copilot-go-hook)
(defun copilot-java-hook ()
  (define-key java-mode-map (kbd "C-c C-x C-k") 'copilot-complete))
(add-hook 'java-common-hook 'copilot-java-hook)

(global-set-key (kbd "C-c C-x C-k") 'copilot-complete)
(global-set-key (kbd "C-c C-x C-e") 'copilot-expert)
(global-set-key (kbd "C-c C-x C-y") 'copilot-robin)

(global-set-key (kbd "C-c C-x C-r") 'copilot-reset-history)

(provide 'copilot)

;;; ansi-mode.el ends here
