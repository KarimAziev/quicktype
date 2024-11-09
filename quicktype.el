;;; quicktype.el --- Generate typescript interfaces from JSON -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/quicktype
;; Version: 0.1.0
;; Keywords: languages tools
;; Package-Requires: ((emacs "27.1") (transient "0.5.3"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Generate typescript interfaces from JSON.

;;; Commands

;; M-x `quicktype'
;;      Dispatch transient popup with available actions.

;;; Customization

;; `quicktype-default-switches'
;;      Alist of default quicktype switches.

;; `quicktype-lang-modes-alist'
;;      Alist of quicktype languages and modes.

;; `quicktype-url-reader'
;;      URL reader should three args - prompt, initial input and history.


;;; Code:

(eval-and-compile
  (require 'cc-mode))
(require 'transient)


(defcustom quicktype-url-reader 'read-string
  "URL reader should three args - prompt, initial input and history."
  :group 'quicktype
  :type 'function)

(defcustom quicktype-lang-modes-alist '(("ts" . typescript-mode)
                                        ("js" . js-mode)
                                        ("c++" . c++-mode)
                                        ("javascript-prop-types" . js-mode)
                                        ("ruby" . ruby-mode)
                                        ("py" . python-mode))
  "Alist of quicktype languages and modes."
  :group 'quicktype
  :type '(alist :key-type (choice :tag "Language"
                                  (const "cs")
                                  (const "go")
                                  (const "rs")
                                  (const "cr")
                                  (const "c++")
                                  (const "objc")
                                  (const "java")
                                  (const "ts")
                                  (const "js")
                                  (const "javascript-prop-types")
                                  (const "flow")
                                  (const "swift")
                                  (const "kotlin")
                                  (const "elm")
                                  (const "schema")
                                  (const "ruby")
                                  (const "dart")
                                  (const "py")
                                  (const "pike")
                                  (const "haskell"))
                :value-type function))

(defcustom quicktype-default-switches '("--just-types")
  "Alist of default quicktype switches."
  :group 'quicktype
  :type '(repeat string))

(defun quicktype-get-region ()
  "Return current active region as string or nil."
  (when
      (and (region-active-p)
           (use-region-p))
    (string-trim (buffer-substring-no-properties
                  (region-beginning) (region-end)))))

(defvar quicktype-js-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?`"\"" table)
    table)
  "Syntax table for JavaScript, including modifications for specific characters.")

(defun quicktype--syntax-propertize-regexp (end)
  "Highlight JavaScript import regex as string syntax.

Argument END is the position in the buffer up to which the syntax propertize
function should apply."
  (let ((ppss (syntax-ppss)))
    (when (eq (nth 3 ppss) ?/)
      (goto-char (nth 8 ppss))
      (when (looking-at
             "/\\(?:[^/[\\]\\|\\\\.\\|\\[\\(?:[^]\\]\\|\\\\.\\)*]\\)*\\(/?\\)")
        (when (> end (match-end 1))
          (setq end (match-end 1)))
        (put-text-property (match-beginning 1) end
                           'syntax-table (string-to-syntax "\"/"))
        (goto-char end)))))

(defun quicktype--syntax-propertize (start end)
  "Highlight JavaScript import syntax elements.

Argument START is the position in the buffer from which to start syntax
propertization.

Argument END is the position in the buffer at which to stop syntax
propertization."
  (goto-char start)
  (quicktype--syntax-propertize-regexp end)
  (funcall
   (syntax-propertize-rules
    ("\\(?:^\\|[=([{,:;|&!]\\|\\_<return\\_>\\)\\(?:[ \t]\\)*\\(/\\)[^/*]"
     (1 (ignore
         (forward-char -1)
         (when (or (not (memq (char-after (match-beginning 0)) '(?\s ?\t)))
                   (save-excursion
                     (goto-char (match-beginning 0))
                     (forward-comment (- (point)))
                     (memq (char-before)
                           (eval-when-compile (append "=({[,:;" '(nil))))))
           (put-text-property (match-beginning 1)
                              (match-end 1)
                              'syntax-table (string-to-syntax "\"/"))
           (quicktype--syntax-propertize-regexp end)))))
    ("\\`\\(#\\)!" (1 "< b")))
   (point) end))

(defmacro quicktype-with-temp-js-buffer (&rest body)
  "Evaluate BODY in temporarily buffer with JavaScript syntax."
  `(with-temp-buffer
     (erase-buffer)
     (progn
       (set-syntax-table quicktype-js-syntax-table)
       (setq-local open-paren-in-column-0-is-defun-start nil)
       (setq-local syntax-propertize-function #'quicktype--syntax-propertize)
       (setq-local parse-sexp-ignore-comments t)
       (setq-local comment-start "// ")
       (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
       (setq-local comment-end "")
       (syntax-ppss-flush-cache (point-min))
       (quicktype--syntax-propertize
        (point-min)
        (point-max))
       ,@body)))

(defun quicktype--f-parent (path)
  "Return the parent directory to PATH without slash."
  (let ((parent (file-name-directory
                 (directory-file-name
                  (expand-file-name path default-directory)))))
    (when (and (file-exists-p path)
               (file-exists-p parent)
               (not (equal
                     (file-truename (directory-file-name
                                     (expand-file-name path)))
                     (file-truename (directory-file-name
                                     (expand-file-name parent))))))
      (if (file-name-absolute-p path)
          (directory-file-name parent)
        (file-relative-name parent)))))

(defun quicktype-momentary-popup-minibuffer-select-window ()
  "Select minibuffer window if it is active."
  (when-let* ((wind (active-minibuffer-window)))
    (select-window wind)))

(defun quicktype-show (content display-action &rest setup-args)
  "Display CONTENT in popup window.

SETUP-ARGS can includes keymaps, syntax table, filename and function.

A function will be called without args inside quit function.

DISPLAY-ACTION can be `display-buffer-in-direction',
 `display-buffer-same-window' etc.
See `(elisp) Buffer Display Action Alists' for details."
  (let ((buffer (get-buffer-create "*quicktype*"))
        (keymaps (seq-filter #'keymapp setup-args))
        (stx-table (seq-find #'syntax-table-p setup-args))
        (mode-fn (seq-find #'functionp setup-args)))
    (with-current-buffer buffer
      (with-current-buffer-window
          buffer
          (cons display-action
                '((window-height . window-preserve-size)))
          (lambda (window _value)
            (with-selected-window window
              (setq buffer-read-only t)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (progn
                  (save-excursion
                    (insert content))
                  (add-hook 'kill-buffer-hook
                            #'quicktype-momentary-popup-minibuffer-select-window
                            nil t)
                  (when mode-fn
                    (funcall mode-fn))
                  (use-local-map
                   (let ((map (make-sparse-keymap)))
                     (if buffer-read-only
                         (define-key map (kbd "q")
                                     #'kill-current-buffer)
                       (define-key map (kbd "q")
                                   #'self-insert-command))
                     (add-hook
                      'read-only-mode-hook
                      (lambda ()
                        (if buffer-read-only
                            (define-key map (kbd "q")
                                        #'kill-current-buffer)
                          (define-key map (kbd "q")
                                      #'self-insert-command)))
                      t)
                     (when keymaps
                       (setq map (make-composed-keymap
                                  keymaps
                                  map)))
                     (set-keymap-parent map (current-local-map))
                     map))))))
        (insert content))
      (when stx-table
        (set-syntax-table stx-table))
      (unless (active-minibuffer-window)
        (select-window (get-buffer-window buffer))))))

(defun quicktype-try-json-from-string (str &optional on-error)
  "Return STR if it is json object or invoke ON-ERROR."
  (require 'json)
  (condition-case err
      (prog1 str
        (when (fboundp 'json-read-from-string)
          (json-read-from-string str)))
    (error
     (when on-error
       (funcall on-error err)))))

(defun quicktype--prettier-format-string (string &rest options)
  "Apply prettier on STRING with OPTIONS.
Return list of two elements: status (t or nil) and string with result."
  (when-let* ((prettier-cmd (let ((dir default-directory)
                                 (node-modules)
                                 (found))
                             (while
                                 (setq node-modules
                                       (unless found
                                         (setq dir
                                               (locate-dominating-file
                                                dir
                                                "node_modules"))))
                               (setq dir (quicktype--f-parent
                                          dir))
                               (let ((file
                                      (expand-file-name
                                       "node_modules/.bin/prettier"
                                       node-modules)))
                                 (setq found
                                       (when (and (file-exists-p file)
                                                  (file-executable-p file))
                                         file))))
                             (or found (executable-find "prettier")))))
    (with-temp-buffer
      (insert string)
      (when (eq 0
                (apply #'call-process-region
                       (append
                        (list (point-min)
                              (point-max)
                              prettier-cmd
                              t
                              t
                              nil)
                        (flatten-list options))))
        (buffer-string)))))

(defun quicktype-find-json-in-kill-ring ()
  "Find and format with prettier json in kill ring."
  (let ((json-formatted)
        (items (seq-filter (apply-partially #'string-match-p "^{") kill-ring))
        (curr))
    (while (and (not json-formatted)
                (setq curr (pop items)))
      (setq json-formatted
            (quicktype--prettier-format-string curr
                                                  "--parser"
                                                  "json")))
    json-formatted))

(defun quicktype-run-with-args (command &rest args)
  "Execute COMMAND with ARGS in PROJECT-DIR.
If DIRECTORY doesn't exists, create new.
Invoke CALLBACK without args."
  (let* ((buff-name (generate-new-buffer-name command))
         (buffer (get-buffer-create buff-name))
         (proc))
    (progn (switch-to-buffer buffer)
           (with-current-buffer buffer
             (setq proc (apply #'start-process buff-name buffer
                               command
                               args))
             (require 'shell)
             (when (fboundp 'shell-mode)
               (shell-mode))
             (view-mode +1))
           (set-process-sentinel
            proc
            (lambda (process _state)
              (let ((output (if (process-buffer process)
                                (with-current-buffer
                                    (process-buffer process)
                                  (buffer-string)))))
                (kill-buffer (process-buffer process))
                (if (= (process-exit-status process) 0)
                    (progn
                      (if (not (string-empty-p
                                (string-trim output)))
                          (quicktype-show
                           output
                           'display-buffer-same-window
                           (cdr (assoc (quicktype-get-arg-value "--lang" args)
                                       quicktype-lang-modes-alist)))
                        (message "finished")))
                  (user-error "%s\n%s" command output)))))
           (require 'comint)
           (when (fboundp 'comint-output-filter)
             (set-process-filter proc #'comint-output-filter)))))

(defun quicktype-string (string &optional args)
  "Execute quicktype on STRING with ARGS."
  (when-let* ((cmd (executable-find "quicktype")))
    (with-temp-buffer
      (insert string)
      (let ((status (apply
                     #'call-process-region
                     (append
                      (list (point-min)
                            (point-max)
                            cmd
                            t
                            t
                            nil)
                      args))))
        (if (eq status 0)
            (quicktype-show (buffer-string)
                            'display-buffer-in-direction
                            (cdr (assoc (quicktype-get-arg-value "--lang" args)
                                        quicktype-lang-modes-alist)))
          (quicktype-show (buffer-string)
                          'display-buffer-in-direction))))))

(defun quicktype--json-from-region ()
  "Extract JSON from selected region and process it with Node.js."
  (when-let* ((reg (quicktype-get-region)))
    (quicktype-try-json-from-string
     reg
     (lambda (&rest _ignored)
       (quicktype-with-temp-js-buffer
        (insert reg)
        (when-let* ((end (progn
                           (re-search-backward
                            "[}]" nil t 1)
                           (forward-char 1)
                           (point)))
                    (start (progn (forward-sexp -1)
                                  (point)))
                    (node (executable-find "node"))
                    (code
                     (prin1-to-string
                      (format
                       "JSON.stringify(%s)"
                       (buffer-substring-no-properties
                        start
                        end))
                      t)))
          (let ((status (call-process node nil t nil "-p" code)))
            (when (zerop status)
              (string-trim (buffer-string))))))))))

(defun quicktype-get-input-json ()
  "Search for json in active region or `kill-ring'.
If not found, return buffer string."
  (or
   (quicktype--json-from-region)
   (quicktype-find-json-in-kill-ring)
   (buffer-substring-no-properties (point-min)
                                   (point-max))))

(defun quicktype-get-arg-value (argument args)
  "Return next element after ARGUMENT in ARGS."
  (car (seq-drop (member argument args) 1)))

;;;###autoload
(defun quicktype-run ()
  "Run quicktype with transient arguments."
  (interactive)
  (unless (executable-find "quicktype")
    (user-error "Cannot find quicktype"))
  (let* ((args (quicktype--get-arguments))
         (input-args (seq-intersection '("--src-urls"
                                         "--src")
                                       args))
         (src-lang (quicktype-get-arg-value"--src-lang" args))
         (lang (quicktype-get-arg-value "--lang" args))
         (out-file (quicktype-get-arg-value "--out" args)))
    (cond
     ((and
       (not input-args)
       (not out-file)
       (equal src-lang "json")
       (equal lang "ts"))
      (quicktype-string (quicktype-get-input-json) args))
     ((and
       (not input-args)
       (not out-file))
      (quicktype-string
       (or (quicktype-get-region)
           (buffer-substring-no-properties (point-min) (point-max)))
       args))
     (t
      (apply #'quicktype-run-with-args "quicktype" args)))))

(transient-define-argument quicktype--additional-schema ()
  "Register the $id's of additional JSON.Schema files."
  :description "Register the $id's of additional"
  :argument "--additional-schema "
  :reader 'transient-read-file
  :class 'transient-option)

(transient-define-argument quicktype--http-method ()
  "HTTP method to use for the GraphQL introspection query."
  :description "HTTP method to use for the GraphQL"
  :argument "--http-method "
  :choices '("POST" "GET")
  :class 'transient-option)

(transient-define-argument quicktype--http-header ()
  "Header(s) to attach to all HTTP requests, including the GraphQL introspection query."
  :description "Header(s) to attach to all HTTP"
  :argument "--http-header "
  :class 'transient-option)

(transient-define-argument quicktype--graphql-schema ()
  "GraphQL introspection file."
  :description "GraphQL introspection file"
  :reader 'transient-read-file
  :argument "--graphql-schema "
  :class 'transient-option)

(transient-define-argument quicktype--graphql-introspect ()
  "Introspect GraphQL schema from a server."
  :description "Introspect GraphQL schema from"
  :argument "--graphql-introspect "
  :class 'transient-option
  :reader quicktype-url-reader)

(defun quicktype-read-src (prompt initial-input history)
  "Read file, url or directory with PROMPT.
INITIAL-INPUT and HISTORY used only for url."
  (let* ((actions `((?f "file" read-file-name ,prompt)
                    (?u "url")
                    (?d "directory" read-directory-name ,prompt)))
         (answer (read-multiple-choice "Type" actions))
         (action (caddr answer)))
    (pcase (car answer)
      (?u "url"
          (funcall quicktype-url-reader prompt
                   initial-input
                   history))
      (_ (apply action (seq-drop answer 3))))))

(transient-define-argument quicktype--src ()
  "The file, url, or data directory to type."
  :description "The file, url, or data directory"
  :argument "--src "
  :always-read t
  :class 'transient-option
  :reader 'quicktype-read-src)

(transient-define-argument quicktype--src-urls ()
  "Tracery grammar describing URLs to crawl."
  :description "Tracery grammar describing URLs"
  :argument "--src-urls "
  :reader quicktype-url-reader
  :class 'transient-option)

(transient-define-argument quicktype--top-level ()
  "The name for the top level type."
  :description "The name for the top level type"
  :argument "--top-level "
  :class 'transient-option
  :always-read t)

(transient-define-argument quicktype--out ()
  "The output file."
  :description "The output file"
  :argument "--out "
  :reader 'transient-read-file
  :class 'transient-option)

(transient-define-argument quicktype--lang ()
  "Target language for quicktype."
  :argument "--lang "
  :description "Target language"
  :always-read t
  :init-value (lambda (obj)  (oset obj value "ts"))
  :class'transient-option
  :choices '("cs" "go" "rs" "cr" "c++" "objc" "java" "ts" "js"
             "javascript-prop-types" "flow" "swift" "kotlin"
             "elm" "schema" "ruby" "dart" "py" "pike" "haskell"))

(transient-define-argument quicktype--src-lang ()
  "Source language for quicktype."
  :argument "--src-lang "
  :description "Source language"
  :class 'transient-option
  :init-value (lambda (obj)  (oset obj value "json"))
  :always-read t
  :choices '("json" "schema" "graphql" "postman" "typescript"))

(defun quicktype--get-arguments ()
  "Return arguments for quicktype."
  (let ((args (transient-args transient-current-command)))
    (flatten-list
     (mapcar (lambda (str)
               (if-let* ((space-pos (string-match-p "[\s\t]" str)))
                   (list (substring str 0 space-pos)
                         (substring str (1+ space-pos)))
                 str))
             args))))

;;;###autoload (autoload 'quicktype "quicktype" nil t)
(transient-define-prefix quicktype ()
  "Transient menu for `quicktype'."
  :value quicktype-default-switches
  ["Common Arguments"
   ("l" quicktype--lang)
   ("s" quicktype--src-lang)
   ("n" quicktype--top-level)
   ("o" quicktype--out)
   ("Sr" quicktype--src)
   ("Sc" quicktype--additional-schema)
   ("ur" quicktype--src-urls)
   ("hm" quicktype--http-method)
   ("hh" quicktype--http-header)
   ("gr" quicktype--graphql-schema)
   ("gi" quicktype--graphql-introspect)]
  ["Common Switches"
   ("-a" " Alphabetize order of class properties" "--alphabetize-properties")
   ("-p" " Make all class properties optional" "--all-properties-optional")
   ("-n" " Don't convert stringified integers" "--no-integer-strings")
   ("-j" " Treat $ref as a reference in JSON" "--no-ignore-json-refs")
   ("-m" " Don't infer maps, always use classes" "--no-maps")
   ("-N" " Don't infer enums, always use strings" "--no-enums")
   ("-d" " Don't convert UUIDs to UUID objects" "--no-uuids")
   ("-O" " Don't infer dates or times" "--no-date-times")
   ("-b" " Don't convert stringified booleans" "--no-boolean-strings")
   ("-c" " Don't combine similar classes" "--no-combine-classes")
   ("-q" " Don't show issues in the generated" "--quiet")]
  ["Typescript arguments"
   ("i" "Interfaces only" "--just-types")
   ("pc" "Transform property names to be JavaScripty" "--nice-property-names")
   ("-ex" "Explicitly name unions" "--explicit-unions")
   ("-rt" "Verify JSON.parse results at" "--runtime-typecheck")
   ("-fu" "Use union type instead of enum" "--prefer-unions")
   ("-iu" "Ignore unknown properties when verifying at runtime"
    "--runtime-typecheck-ignore-unknown-properties")
   ("c" "Converters" "--converters "
    :class transient-option
    :choices ("top-level" "all-objects"))
   ("ac" "Acronym naming style" "--acronym-style "
    :class transient-option
    :choices ("original" "pascalcamel" "lowerCase"))]
  ["Actions"
   ("RET" "Run" quicktype-run)
   ("<return>" "Run" quicktype-run)])

(provide 'quicktype)
;;; quicktype.el ends here