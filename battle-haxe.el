;;; battle-haxe.el --- A Haxe development system, with code completion and more ;; -*- lexical-binding: t -*-

;; Copyright (C) 2019-2019  Alon Tzarafi

;; Author: Alon Tzarafi  <alontzarafi@gmail.com>
;; URL: https://github.com/AlonTzarafi/battle-haxe
;; Version: 1.0
;; Package-Requires: ((emacs "25") (company "0.9.9") (helm "3.0") (async "1.9.3") (dash "2.12.0") (cl-lib "0.5") (s "1.10.0") (f "0.19.0"))
;; Keywords: programming, languages, completion

;; battle-haxe is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; battle-haxe is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with battle-haxe.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This package offers a development system for the Haxe programming language.
;; Haxe code completion is activated using the `company-mode' package.
;; Options like "go to definition" and "find all references" are available, as well as `eldoc' support.
;; All of those features are triggered in `battle-haxe-services-mode' which also spawns a Haxe server to perform them.
;; The tools rely on the Haxe "compiler services" feature ( https://haxe.org/manual/cr-completion-overview.html ).
;; The main quirk is that the system has to force automatic saving of the edited Haxe buffer.
;; If this is a problem for you don't use the package.
;; A `battle-haxe-syntax-mode' is supplied but it actually uses `js-mode' for syntax highlighting and indentation.
;; See the project home page for more information.

;;; Code:

;; For completion
(require 'company)
;; For syntax highlighting and indentation
(require 'js)
;; For find references
(require 'helm)
;; For non-blocking operations
(require 'async)
;; For helper functions
;;  (built in)
(require 'cl-lib)
(require 'subr-x)
(require 'xml)
;;  (downloaded)
(require 'dash)
(require 's)
(require 'f)

(defvar battle-haxe-cwd nil
  "Current working directory, or current project folder to pass to Haxe.")

(defvar battle-haxe-cached-hxml-args ""
  "Holds the Haxe compiler parameters, extracted from the '.hxml' file.
They are sent to the compiler to call compiler services for this project.")

(defvar battle-haxe-compiler-port 6000
  "The port that the Haxe server and client communicate with.")

(defvar battle-haxe-server-process nil
  "References a currently running Haxe server process if any.")

(defvar battle-haxe-services-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used for symbol `battle-haxe-services-mode'.")

;;;###autoload
(define-derived-mode battle-haxe-syntax-mode js-mode "Haxe"
  "Haxe syntax highlighting mode. This is simply using js-mode for now.")

;;;###autoload
(define-minor-mode battle-haxe-services-mode
  "Haxe completion, and other coding tools, using Haxe's compiler services."
  :lighter " haxe"
  :global nil
  :keymap battle-haxe-services-mode-map
  ;;Init the minor mode
  (progn
    ;; Setup company for Haxe, and start server if needed
    (battle-haxe-init)
    
    ;; Setup eldoc for Haxe
    (set (make-local-variable 'eldoc-documentation-function)
         #'battle-haxe-eldoc-function)))

;;;###autoload
(defun battle-haxe-company-backend (command &optional arg &rest ignored)
  "'company-mode' completion back-end using Haxe compiler services. Backend is called using COMMAND, ARG, and the rest is IGNORED."
  (interactive '(interactive))
  
  (cl-case command
    (interactive (company-begin-backend #'battle-haxe-company-backend))

    ;; [[file+here:~/.emacs.d/elpa/omnisharp-20181206.21/omnisharp-auto-complete-actions.el::180]]
    (prefix
     (if
         (and (bound-and-true-p battle-haxe-services-mode)
              (not (company-in-string-or-comment)))
         (cons ""
               ;; (battle-haxe-inserted-member)
               t)
       nil))
    
    (candidates (battle-haxe-candidates))
    
    ;; TODO: Test if can get it working without this
    (no-cache t)
    
    (sorted t)
    
    (annotation (battle-haxe-annotation arg))
    
    (meta (battle-haxe-get-prop arg 'meta))
    
    (require-match 'never)
    
    (doc-buffer (battle-haxe-docstring arg))
    
    (match (battle-haxe-get-prop arg 'match))
    
    (post-completion
     ;; Clean up the old inserted text
     (backward-char (length arg))
     (delete-char (- (battle-haxe-get-prop arg 'matchlen)))
     (forward-char (length arg))
     ;; After "new MyClass()" completion also auto-import it
     (battle-haxe-ensure-object-at-point-is-imported (battle-haxe-get-prop arg 'ensure-import))))
  )

(defun battle-haxe-get-haxe-point (&optional target-point filename)
  "Return a haxe-point to be passed to Haxe's --display option.
A so called haxe-point looks like this: path-to-file/Haxe-file.hx@bytepos
The result is based either on FILENAME and TARGET-POINT,
or, if they are nil, on the current buffer file and point."
  (let ((target-point (or target-point (point) )))
    (concat
     (or filename buffer-file-name)
     "@"
     (number-to-string
      (battle-haxe-get-byte-pos (or target-point (point)))))))

(defun battle-haxe-get-byte-pos (target-point)
  "Take the TARGET-POINT in the current buffer and return the byte position.
Haxe compiler services damands only byte position so some quirks must be handled."
  (let ((was-multibyte enable-multibyte-characters)
        (bytepos 0))
    (set-buffer-multibyte nil)
    (setq bytepos
          (-
           (bufferpos-to-filepos target-point 'exact)
           (if (battle-haxe-file-has-BOM)
               2
             0)))
    (set-buffer-multibyte was-multibyte)
    bytepos))

(defun battle-haxe-file-has-BOM ()
  "Detect if current file has a byte order mark (BOM) at the beginning."
  (let ((filename buffer-file-name))
    (if filename
        (with-temp-buffer
          (insert-file-contents-literally filename)
          (let ((bom (decode-coding-string (buffer-substring-no-properties 1 4) 'utf-8)))
            (string= bom (string ?\uFEFF)))))))

(defun battle-haxe-ensure-object-at-point-is-imported (full-class-name)
  "Maybe add an import statement in the current buffer to the FULL-CLASS-NAME.
Only adds the import if the class/type is not already available to the file.
When calling, the point must be on a class reference to that class.
The Haxe compiler services is then called to indentify the class."
  
  (when full-class-name
    (battle-haxe-save-buffer-silently)
    
    (let* ((is-imported
            (save-excursion
              (goto-char (point-min))
              (re-search-forward (concat "^[ \t]*import[ \t]+" full-class-name "[ \t]*;") nil t)))
           (needs-to-be-imported
            (and
             ;; not imported directly:
             (not is-imported)
             ;; and also not found by compiler services:
             (battle-haxe-is-invalid-pos-string (car (battle-haxe-position-query (battle-haxe-get-haxe-point)))))))
      (if needs-to-be-imported
          (save-excursion
            (goto-char 0)
            (while (re-search-forward "package[ \t[:alnum:]];" nil t))
            (while (re-search-forward "import[ \t]+.+[ \t]*;" nil t))
            (beginning-of-line)
            (unless (equal (point-at-bol) (point-at-eol))
              (forward-line))
            (open-line 1)
            (insert (concat "import " full-class-name ";"))
            (message (concat "Class added to import list: " full-class-name)))))))

(defun battle-haxe-candidates ()
  "The main company-completion function.
Can complete either a Haxe class/object's member or a Haxe type.
Checks already entered text to match with target member/type.
Asynchonously send a list of candidates to the 'company-mode'."
  (when buffer-file-name
    (let*
        ((is-done? nil)
         
         (inserted-member (unless is-done? (battle-haxe-inserted-member)))
         (is-done? (or is-done? inserted-member))
         (inserted-type (unless is-done? (battle-haxe-inserted-type)))
         (is-done? (or is-done? inserted-type))
         
         (completions-parser nil)
         (inserted-text nil)
         (haxe-compiler-services-mode nil)
         
         ;;choose completion type:
         (which-completion
          (cond
           (inserted-member
            (progn
              (setq completions-parser #'battle-haxe-member-completions-from-xml)
              (setq inserted-text inserted-member)
              ;; (setq haxe-compiler-services-mode "@position")
              'complete-member-or-type))
           (inserted-type
            (progn
              (setq completions-parser #'battle-haxe-type-completions-from-xml)
              (setq inserted-text inserted-type)
              ;; (setq haxe-compiler-services-mode "@type")
              'complete-member-or-type))
           (t 'none))))

      is-done?                          ;Yeah... It's done
      
      (case which-completion
        (complete-member-or-type
         (let* ((inserted-member-begin (- (point) (length inserted-text)))
                (haxe-point (battle-haxe-get-haxe-point inserted-member-begin)))
           
           ;; let Haxe compiler read this file before proceeding to call the server
           (battle-haxe-save-buffer-silently)
           
           (battle-haxe-company-async-candidates
            haxe-point
            haxe-compiler-services-mode
            completions-parser
            inserted-text)))
        (none
         nil)))))

(defun battle-haxe-company-async-candidates (haxe-point haxe-compiler-service completions-parser inserted-text)
  "Return a cons pair that company accepts as an async completion.
HAXE-POINT is the haxe-point.
HAXE-COMPILER-SERVICE is haxe compiler services mode (example: '@type').
COMPLETIONS-PARSER is a function that transforms the XML result string
into candidates.
INSERTED-TEXT is sent to the parser to help match some candidates."
  (let ((process-result
         (lambda (xml-str)
           ;; Got server results
           (funcall completions-parser xml-str inserted-text))))
    (cons :async (lambda (callback)
                   (battle-haxe-get-completions
                    haxe-point
                    haxe-compiler-service
                    (lambda (xml-str)
                      (funcall callback (funcall process-result xml-str))))))))

(defun battle-haxe-get-completions (haxe-point haxe-compiler-service callback)
  "Call a compiler services completion command and pass result to CALLBACK.
The command is called in HAXE-POINT using the mode in HAXE-COMPILER-SERVICE."
  (let* ((command-to-call (battle-haxe-make-command-string haxe-point haxe-compiler-service))
         (command-result (shell-command-to-string
                          command-to-call)))
    ;; When done:
    (funcall callback command-result)))

(defun battle-haxe-resolve-hxml-candidates ()
  "Resolves a list of .hxml file candidates and directories to be used.
for starting a server based on the current buffer file's directory"
  (let ((dir (file-name-directory (or buffer-file-name "")))
        (candidates nil))
    (while (and dir (not (f-root-p dir)))
      (if (not (file-remote-p dir))
          (setq candidates
                (append candidates
                        (f-files dir (lambda (filename)
                                       (string-match-p "\\.hxml$" filename))))))
      (setq dir (f-parent dir)))
    
    (setq candidates (reverse candidates))
    
    ;; Another idea: use project root as a candidate (if have projectile available)
    
    candidates))

(defun battle-haxe-read-lines (file-path)
  "Return a list of lines of a file at FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun battle-haxe-additional-compiler-flags ()
  "Return the cached hxml args plus an argument specifying the current working directory."
  (append
   (list
    "--cwd" battle-haxe-cwd)
   battle-haxe-cached-hxml-args))

(defun battle-haxe-make-command-string (haxe-point compiler-services-mode)
  "Construct a shell command for calling compiler services on the Haxe server.
The HAXE-POINT specifies the position of the operation.
the COMPILER-SERVICES-MODE is appended to the haxe-point in the --display argument."
  (s-join
   " "
   (append
    (list
     "haxe"
     "--connect"
     (number-to-string battle-haxe-compiler-port))
    (battle-haxe-additional-compiler-flags)
    (list
     "--display"
     (concat
      haxe-point
      (concat "@" compiler-services-mode))))))

(defun battle-haxe-get-xml-root (xml-string)
  "Get a root XML object from the provided XML-STRING."
  (with-temp-buffer
    (progn
      (insert xml-string)
      (ignore-errors
        (xml-parse-region (point-min) (point-max))))))

(defun battle-haxe-member-completions-from-xml (xml-str inserted-text)
  "Parse the XML-STR string returned from the Haxe server.
Combine with the INSERTED-TEXT at the completion point,
to produce member completion candidates."
  (battle-haxe-completions-from-xml-common
   xml-str
   #'battle-haxe-member-completion-xml-node-processor
   inserted-text))

(defun battle-haxe-type-completions-from-xml (xml-str inserted-text)
  "Parse the XML-STR string returned from the Haxe server.
Combine with the INSERTED-TEXT at the completion point,
to produce type completion candidates."
  (battle-haxe-completions-from-xml-common
   xml-str
   #'battle-haxe-type-completion-xml-node-processor
   inserted-text))

(defun battle-haxe-completions-from-xml-common (xml-str completion-node-processor inserted-text)
  "Parse the XML-STR string returned from the Haxe server.
Use COMPLETION-NODE-PROCESSOR on each child node of the XML.
Send INSERTED-TEXT to the node processor to help match candidtes.
Sort the candidates according to those matches.
The result is the full list of processed completion candidates."
  (let*
      ((xml-root (battle-haxe-get-xml-root xml-str))
       (completion-xml-nodes (xml-get-children (car xml-root) 'i))
       (calc-completion-xml-node
        (lambda (xml-node)
          (funcall completion-node-processor xml-node inserted-text)))
       (completions-processed
        (mapcar calc-completion-xml-node completion-xml-nodes))
       (completions-filtered
        (seq-filter #'stringp completions-processed))
       (completions-sorted (sort
                            completions-filtered
                            (lambda (a b)
                              (>
                               (battle-haxe-get-prop a 'matchlen 0)
                               (battle-haxe-get-prop b 'matchlen 0))
                              )))
       (completions completions-sorted))
    completions))

(defun battle-haxe-member-completion-xml-node-processor (completion-xml-node inserted-text)
  "The node processor for performing member completion.
Take COMPLETION-XML-NODE,
an individual result of compiler services member completion.
Parse it and return a string of the candidate name,
plus some meta-data as text properties.
The completion candidate is matched to INSERTED-TEXT."
  (let*
      ((competion-attrs (xml-node-attributes completion-xml-node))
       (kind (cdr (assoc 'k competion-attrs)))
       (type (or (car (cdr (cdr (nth 0 (xml-get-children completion-xml-node 't)))))
                 ""))
       
       (name (battle-haxe-set-string-face
              'apropos-function-button
              (concat
               (cdr (assoc 'n competion-attrs))
               (if (string= kind "method")
                   (car (battle-haxe-split-function-type type))
                 ""))))
       
       ;; (error-check (unless (stringp name)
       ;;                (error "Error. Couldn't parse completion node's name")))
       
       (docstring (car (cdr (cdr (nth 0 (xml-get-children completion-xml-node 'd))))))
       (match (battle-haxe-calc-match inserted-text name))
       ;;NOTE: change this if matching smartly to multiple sections
       (matchlen
        (if match
            (- (cdr (first match)) (car (first match)))
          0)
        )
       (output
        name))
    (battle-haxe-set-prop output 'kind kind)
    (battle-haxe-set-prop output 'type type)
    (battle-haxe-set-prop output 'docstring docstring)
    (battle-haxe-set-prop output 'match match)
    (battle-haxe-set-prop output 'matchlen matchlen)
    output))

(defun battle-haxe-type-completion-xml-node-processor (completion-xml-node inserted-text)
  "The node processor for performing type completion.
Take COMPLETION-XML-NODE,
an individual result of compiler services member completion.
Parse it and return a string of the candidate name,
plus some meta-data as text properties.
The completion candidate is matched to INSERTED-TEXT."
  (let*
      ((competion-attrs (xml-node-attributes completion-xml-node))
       (name (car (xml-node-children
                   completion-xml-node)))
       (kind "")
       (type (cdr (assoc 'p competion-attrs)))
       (ensure-import type)             ;Used later for auto-import
       (docstring "")
       (match (battle-haxe-calc-match inserted-text name))
       (matchlen
        (if match
            (- (cdr (first match)) (car (first match)))
          0))
       (output name))
    (battle-haxe-set-prop output 'kind kind)
    (battle-haxe-set-prop output 'type type)
    (battle-haxe-set-prop output 'ensure-import ensure-import)
    (battle-haxe-set-prop output 'docstring docstring)
    (battle-haxe-set-prop output 'match match)
    (battle-haxe-set-prop output 'matchlen matchlen)
    output))

(defun battle-haxe-set-prop (completion property value)
  "Put a text-property PROPERTY with value VALUE in string COMPLETION."
  (if completion
      (put-text-property 0 1 property value completion)))

(defun battle-haxe-get-prop (completion property &optional default-value)
  "Get a text-property PROPERTY in string COMPLETION.
Optionally provide a DEFAULT-VALUE if not found."
  (if-let (property (get-text-property 0 property completion))
      property
    default-value))

(defun battle-haxe-annotation (name)
  "Annotate a completion-candidate NAME accodring to the attached meta-data."
  (let ((kind (get-text-property 0 'kind name))
        (type (get-text-property 0 'type name)))
    (if (and (string= kind "method") (string-match-p "->" name))
        (progn
          (let* ((split (battle-haxe-split-function-type type))
                 ;; (args (car split))
                 (return-type (cdr split)))
            (concat
             " : "
             return-type
             )))
      (concat ": " type))))

(defun battle-haxe-split-function-type (function-type)
  "Return a cons pair of (function-args . return-type) strings.
FUNCTION-TYPE is the full function type string,
as reported by Haxe compiler services."
  (string-match "\\(.*\\(?:\\|Void\\).*\\)\\( -> \\)\\(.*\\)" function-type)
  (let* (;; (total-length (length function-type))
         (match-beg (match-beginning 2))
         (valid
          ;; (and (numberp match-beg) (<= match-beg total-length ))
          t
          )
         (arguments-end (if valid match-beg 0))
         (return-type-begin arguments-end)
         (raw-args (substring function-type 0 arguments-end))
         (args
          (if (string= raw-args "Void")
              ;; Force Haxe function hints to always
              ;; display with () parenthesis directly after the function name.
              "()"
            raw-args))
         (return-type (substring function-type return-type-begin)))
    (cons args return-type)))

(defun battle-haxe-set-string-face (face string)
  "Helper to color STRING according to FACE."
  (put-text-property 0 (length string) 'face face string)
  string)

(defun battle-haxe-docstring (name)
  "Fetch documentation string from completion candidate NAME's metadata."
  (let
      ((doc-buffer
        (company-doc-buffer
         (concat
          (replace-regexp-in-string
           ;;remove tabs and spaces at beginning of lines
           "\n[\t ]+"
           "\n"
           (replace-regexp-in-string
            ;; remove empty lines and whitespace before text
            "\\`[ \t\n]*"
            ""
            (battle-haxe-get-prop name 'docstring "No documentation provided\n")))
          
          ;; Give extra character to buffer because popup window crops up my text :-/
          ;; Take note: the following string isn't empty,
          "⁣"                            ; <-- there is a unicode "INVISIBLE SEPARATOR" here
          ))))
    ;; (with-current-buffer doc-buffer
    ;;   (visual-line-mode))
    doc-buffer))

(defun battle-haxe-inserted-member ()
  "Return a typed member name in a Haxe buffer, or nil if not typing a member."
  (let*
      ((beg-of-line
        (save-excursion (beginning-of-line) (point)))
       (inserted-member
        (save-excursion
          (if
              (re-search-backward
               "\\(?:[.]\\|override \\)\\([[:alnum:]\\|_]*\\)\\="
               beg-of-line
               t)
              (match-string 1)
            nil))))
    inserted-member))

(defun battle-haxe-inserted-type ()
  "Return inserted type in syntax that expect types.

Specifically detects lines like this:
case Node(_):
And in those cases their colon is ignored and doesn't count as a type prefix.
However, in lines like this:
function fun():
The colon is accepted as a prefix to a Haxe type (and completion can trigger)."
  (let*
      ((beg-of-line
        (save-excursion (beginning-of-line) (point)))
       (is-switch-case-colon
        (save-excursion
          (if
              (re-search-backward
               "^[[:blank:]]*\\(?:default\\|case\\)\\(.*\\):\\="
               beg-of-line
               t)
              (match-string 1)
            nil)))
       (inserted-member
        (unless is-switch-case-colon ;disable type-completion in situations like "case 1:"
          (save-excursion
            (if
                (re-search-backward
                 "\\(?::\\|[[:blank:]]\\(?:new \\|import \\|using \\|\)\\)\\)\\([[:alnum:]\\|_]*\\)\\="
                 beg-of-line
                 t)
                (match-string 1)
              nil)))))
    inserted-member))

(defun battle-haxe-typed-function()
  "Try to find the position the of 'current' function.
That is, the function you're currently writing arguments to.
If found, return a buffer position that sits in this function's name.
If no function found, return current point."
  (or (let*
          ((beg-of-line
            (save-excursion (beginning-of-line) (point)))
           (pos-at-function
            (save-excursion
              (if
                  (re-search-backward
                   "\\([[:alnum:]]\\)[[:space:]]*[(][^(]*\\="
                   ;;TODO: Maybe later allow multi-line function calls?
                   beg-of-line
                   t)
                  (match-beginning 1)
                nil))))
        pos-at-function)
      (point)))

(defun battle-haxe-calc-match (inserted-text name)
  "Calculate the part of the INSERTED-TEXT that matchs NAME. Used for company candidates."
  (if name
      (let*
          ((str-match (string-match inserted-text name 0))
           (mbeg (if str-match
                     (match-beginning 0)
                   0))
           (mend (if str-match
                     (match-end 0)
                   0)))
        (list (cons mbeg mend)))))

(defun battle-haxe-save-buffer-silently ()
  "Save the buffer immediately before battle-haxe operations.
This forces you to save your documents unfortunately even if you don't want to,
but so far it's the only way I managed to get it to work.
Haxe asks for file paths and will not accept --display requests in temporary files."
  (let ((message-log-max nil))
    (save-buffer)))

(defun battle-haxe-eldoc-function ()
  "Asynchronously launch eldoc with a type hint string.
Contextually determines the current function or other symbol,
and displays the type information data for it.
If nothing found to fetch type information to, do nothing."
  
  (when buffer-file-name
    (battle-haxe-save-buffer-silently)
    
    ;; TODO: Use saved cached data when possible
    
    (battle-haxe-find-function-details
     (lambda (function-xml-details)
       (if
           function-xml-details
           (let*
               ((name (alist-get 'name function-xml-details))
                (root (alist-get 'xml-root function-xml-details))
                (is-function? (alist-get 'is-function? function-xml-details))
                (node-name (xml-node-name (xml-node-children (xml-node-name root))))
                (signature (battle-haxe-first-real-line-of node-name))
                (signature-parts (battle-haxe-string-regex-results "\\(.*\\) -> \\(.*\\)" 2 signature))
                (args (nth 0 signature-parts))
                (fixed-args (if (string= args "Void") "()" args))
                (returnval (nth 1 signature-parts)))
             (if name
                 (eldoc-message
                  (if is-function?
                      (concat
                       ;; node-name fixed-args " : " returnval
                       ;; name "(" args ") : " returnval
                       name fixed-args " : " returnval
                       )
                    (concat
                     name " : " signature
                     ))))))))))

(defun battle-haxe-first-real-line-of (str)
  "Get the first non-empty line in STR."
  (first (-remove #'string-empty-p (split-string str "\n"))))

(defun battle-haxe-get-pos-string-parts (pos-string)
  "Split the returned POS-STRING to an alist with it's components.
the pos-string is returned from Haxe compiler services --display requests,
such as @position."
  (cl-mapcar #'cons '(file line char)
             (battle-haxe-string-regex-results
              "[[:space:]]*\\(.*\\):\\([[:digit:]]*\\): characters \\([[:digit:]]*\\)-[[:digit:]]*"
              3
              pos-string)))

(defun battle-haxe-is-invalid-pos-string (pos-string)
  "Check if this POS-STRING is actually a returned error message from the server."
  (or (not pos-string) (cl-search "unknown" pos-string )))

(defun battle-haxe-find-function-details (callback)
  "Calculate a cons result of (name . xml) about the current function.
That is, the Haxe function you're currently typing or typing arguments to.
The result will be returned to CALLBACK."
  (let ((function-info (battle-haxe-typed-function)))
    (if function-info
        (let* ((function-pos function-info)
               (shell-cmd (battle-haxe-make-command-string
                           (battle-haxe-get-haxe-point function-pos) "position")))
          (async-start
           `(lambda ()
              (shell-command-to-string ,shell-cmd))
           (lambda (haxe-@position-output)
             (let ((pos-string (xml-node-name
                                (xml-node-children
                                 (nth 0
                                      (xml-get-children
                                       (xml-node-name (battle-haxe-get-xml-root
                                                       haxe-@position-output))
                                       'pos))))))
               (if (battle-haxe-is-invalid-pos-string pos-string)
                   (progn
                     ;; (message
                     ;;  (format
                     ;;   "Couldn't find function position. Haxe returned this:\n%s"
                     ;;   haxe-@position-output))
                     nil)
                 (let* ((parts (battle-haxe-get-pos-string-parts pos-string))
                        (filename (alist-get 'file parts))
                        (line (string-to-number (alist-get 'line parts)))
                        (char (string-to-number (alist-get 'char parts))))
                   (let* ((try-function-name (battle-haxe-get-regex-at-coordinates "\\([^( \t\n]*\\)" filename line char))
                          (try-var-name (battle-haxe-get-regex-at-coordinates "\\([^: \t\n]*\\)" filename line char))
                          (is-function? (< (length try-function-name) (length try-var-name)))
                          (result-name)
                          (result-xml-root)
                          (haxe-point (battle-haxe-get-haxe-point-from-coordinates filename line char))
                          (shell-cmd2 (battle-haxe-make-command-string haxe-point "type")))
                     
                     (async-start
                      `(lambda ()
                         (shell-command-to-string ,shell-cmd2))
                      (lambda (haxe-@type-output)
                        
                        (setq result-name (if is-function?
                                              try-function-name
                                            try-var-name))
                        (setq result-xml-root (battle-haxe-get-xml-root haxe-@type-output))
                        
                        ;; pass the final value:
                        (funcall callback
                                 (list
                                  (cons 'name result-name)
                                  (cons 'xml-root result-xml-root)
                                  (cons 'is-function? is-function?)))))))))))
          ;; Found something and trying asynchronously but return nil so eldoc doesn't launch
          nil)
      ;; Not on a function
      nil)))

(defun battle-haxe-get-regex-at-coordinates (regex filename line char)
  "Travel to the file FILENAME in a temporary buffer and extract some text.
The function goes to LINE and CHAR coordinates and tries to match REGEX."
  (let ((temp-buffer-name "*Haxe-temp*"))
    (progn
      (if (not (get-buffer temp-buffer-name))
          (generate-new-buffer temp-buffer-name))
      (with-current-buffer
          temp-buffer-name
        (delete-region (point-min) (point-max))
        (insert-file-contents filename)
        (goto-char 0)
        (forward-line (decf line))
        (forward-char (decf char))
        (let ((result
               (save-excursion
                 (re-search-forward regex nil t)
                 (match-string 1))))
          (kill-buffer temp-buffer-name)
          result)))))

(defun battle-haxe-get-haxe-point-from-coordinates (filename line char)
  "Transforms FILENAME:LINE:CHAR coordinates.
The result is a haxe-point that can be sent to Haxe compiler services."
  (with-temp-buffer
    (insert-file-contents filename)
    
    (goto-char 0)
    (forward-line (decf line))
    (forward-char (decf char))
    (save-excursion
      (re-search-forward "\\([^( \t\n]*\\)" nil t))
    (concat filename (battle-haxe-get-haxe-point (point)))))

(defun battle-haxe-string-regex-results (regex num-results str)
  "Perform REGEX matching on the string STR.
Match a number of capture groups specified by NUM-RESULTS.
All of the matched results are returned as a list."
  (with-temp-buffer
    (insert str)
    (goto-char 0)
    (re-search-forward regex nil t)
    (map 'listp
       (lambda (n) (match-string n))
       (number-sequence 1 num-results))))

(defun battle-haxe-init ()
  "Set up the Haxe compiler services in the buffer.
Start the Haxe compiler in server mode in current project upon loading file.
This will only start the server if no server is running.
If you want to restart, or start in another project, use ‘battle-haxe-start-server’."
  (unless (battle-haxe-is-server-running)
    (battle-haxe-start-server)))

(defun battle-haxe-is-server-running ()
  "Return non-nil if Haxe server is running."
  (and
   battle-haxe-server-process
   (eq  'run (process-status battle-haxe-server-process))))

(defun battle-haxe-start-server ()
  "Forcibly start the Haxe server in the current project.
If the server exists then kill it and start a new one.
Can be called interactively in another project to start a server there instead."
  (interactive)
  
  (when (battle-haxe-is-server-running)
    (kill-process battle-haxe-server-process)
    (setq battle-haxe-server-process nil))
  
  (when-let ((hxml-path
              (first
               (battle-haxe-resolve-hxml-candidates))))
    ;;TODO: Make project-specific vars not global and allow launching several compilation servers, one per project?
    (setq battle-haxe-cached-hxml-args
          (battle-haxe-read-lines hxml-path))
    (setq battle-haxe-cwd
          (file-name-directory hxml-path)))
  
  ;; A delay between closing the server and starting a new one prevents problems.
  (run-at-time
   "1 sec"
   nil
   (lambda ()
     ;; (with-current-buffer (get-buffer-create "*Haxe server*")
     (setq battle-haxe-server-process
           (let ((shell-cmd
                  (s-join
                   " "
                   (list
                    ;; "start haxe -v --wait " (number-to-string battle-haxe-compiler-port)
                    "haxe -v --wait " (number-to-string battle-haxe-compiler-port)
                    " "
                    (s-join
                     " "
                     (battle-haxe-additional-compiler-flags))))))
             
             (async-start
              `(lambda ()
                 (shell-command-to-string
                  ,shell-cmd))))))))

(defun battle-haxe-goto-definition ()
  "Find definition of Haxe symbol at point and visit it."
  (interactive)
  
  (battle-haxe-save-buffer-silently)
  
  (let*
      ((response (battle-haxe-position-query (battle-haxe-get-haxe-point)))
       (pos-string (car response))
       (shell-result (cdr response)))
    (if (battle-haxe-is-invalid-pos-string pos-string)
        (message (format "Could not find definition. Haxe returned this:\n%s" shell-result))
      (battle-haxe-jump-to-pos-string (battle-haxe-get-pos-string-parts pos-string)))))

(defun battle-haxe-position-query (haxe-point)
  "Ask server the original position of object at point HAXE-POINT.
The return is a cons pair of (pos-string . shell-result)"
  (let*
      ((query (battle-haxe-make-command-string haxe-point "position"))
       (shell-result (shell-command-to-string query))
       (pos-string (nth 2 (car
                           (xml-get-children
                            (car (battle-haxe-get-xml-root shell-result))
                            'pos)))))
    (cons pos-string shell-result )))

(defun battle-haxe-jump-to-pos-string (pos-string-parts)
  "Visit coordinates (file, line, char) returned by Haxe compiler services.
POS-STRING-PARTS hold the coordinates.
The function uses 'push-mark' to enable going back to the previous position."
  (let*
      ((parts pos-string-parts)
       (filename (alist-get 'file parts))
       (line (string-to-number (alist-get 'line parts)))
       (char (string-to-number (alist-get 'char parts))))
    (when filename
      (push-mark)
      (find-file filename)
      (goto-char 0)
      (forward-line (decf line))
      (forward-char (decf char)))))

(defun battle-haxe-helm-find-references ()
  "Ask Haxe for all references to symbol at point and prompt to visit one.
Choosing between the results is done with helm."
  (interactive)
  
  (battle-haxe-save-buffer-silently)
  
  (let* ((command-to-call1      ;get the declaration
          (battle-haxe-make-command-string (battle-haxe-get-haxe-point) "position"))
         (command-to-call2      ;get all usage found
          (battle-haxe-make-command-string (battle-haxe-get-haxe-point) "usage"))
         (source-list
          (xml-get-children
           (car (battle-haxe-get-xml-root (shell-command-to-string command-to-call1)))
           'pos))
         (is-valid source-list)
         (usages-list
          (if is-valid
              (xml-get-children
               (car (battle-haxe-get-xml-root (shell-command-to-string command-to-call2)))
               'pos)))
         (candidates
          (if is-valid
              (-map
               (lambda (node)
                 (let* ((pos-string (nth 2 node))
                        (parts (battle-haxe-get-pos-string-parts pos-string))
                        (file (alist-get 'file parts))
                        (formatted
                         (format
                          "%s:%s:%s"
                          (propertize file
                                      'face 'helm-grep-file)
                          (propertize (alist-get 'line parts)
                                      'face 'helm-grep-lineno)
                          (propertize (battle-haxe-get-line-contents parts)
                                      'face 'helm-grep-match)))
                        (candidate
                         (cons formatted parts)))
                   candidate))
               (append
                source-list
                usages-list)))))
    (if candidates
        (helm :sources   `((name . "Sections")
                           (candidates . ,candidates)
                           (volatile)
                           (action . (("Goto `RET'" . battle-haxe-jump-to-pos-string))))
              :buffer "*helm-haxe-find-references*")
      (message "No references found in code."))))

(defun battle-haxe-get-line-contents (parts)
  "Take PARTS (coordinates to a file position) and return the line's string there."
  (let* ((filename (alist-get 'file parts))
         (line (string-to-number (alist-get 'line parts)))
         (char 1))
    (or (battle-haxe-get-regex-at-coordinates
         "\\(?:[[:blank:]]\\)*\\(.*\\)$"
         filename line char)
        "")))

(add-to-list 'company-backends #'battle-haxe-company-backend)

(provide 'battle-haxe)
;;; battle-haxe.el ends here
