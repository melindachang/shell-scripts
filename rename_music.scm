#!/usr/bin/env -S steel --

(#%require-dylib "libsteel_taglib" (only-in get-audio-tags rename!))

(require "srfi/srfi-28/format.scm")
(require "cliron/main.scm")

(define g-supported-audio-file-types '("flac" "mp3"))
(define g-supported-meta-file-types '("cue" "log"))

;; audio-file? : string? -> bool?
;; Returns true if path exists and points to a supported audio file, false otherwise.
(define (audio-file? path)
  (and (member (path->extension path) g-supported-audio-file-types)
       (is-file? path)))
 
;; meta-file? : string? -> bool?
;; Returns true if path exists and points to a supported metadata file, false otherwise.
(define (meta-file? path)
  (and (member (path->extension path) g-supported-meta-file-types)
       (is-file? path)))

;;; UTILS

;; sluggify-str : string? -> string?
(define (sluggify-str s)
  (let* ([blacklist '(#\/ #\\ #\* #\: #\? #\| #\< #\>)]
         [chars (string->list s)]
         [replaced-chars (map (λ (c) (if (member c blacklist) #\_ c)) chars)])
    (list->string replaced-chars)))

;; pad-to-width : string? number? -> string?
;; Pad a string with zeros to the specified width.
(define (pad-to-width s width)
  (if (< (string-length s) width)
      (string-append (make-string (- width (string-length s)) #\0) s)
      s))

;; delete-if-empty! : string? -> void?
(define (delete-if-empty! path)
  (let ([remaining (read-dir path)])
    (when (null? remaining)
      (displayln (format "Cleaning up empty directory: ~a" path))
      (delete-directory! path))))

;; truncate-file-str : string? number? -> string?
(define (truncate-file-str s len)
  (if (> (string-length s) len)
      (string-append (substring s 0 (- len 3)) "...")
      s))

;; get-new-filename : audio-file? -> string?
;; Make new file name for the specified audio file.
(define (get-new-filename path)
  (let* ([ext (path->extension path)]
         [tags (get-audio-tags path)]
         [disc-number (pad-to-width (or (tag-try-get tags 'discnumber) 1) 2)]
         [track-number (pad-to-width (tag-ref tags 'tracknumber) 2)]
         [track-title
          (truncate-file-str (sluggify-str (tag-ref tags 'tracktitle)) 60)])
    (format "~a.~a - ~a.~a" disc-number track-number track-title ext)))

;; get-new-meta-filename : string? -> string?
;; Make new file name for the specified metadata file.
(define (get-new-meta-filename path tags)
  (let* ([set-subtitle (tag-try-get tags 'setsubtitle)]
         [meta-name (if (and (string? set-subtitle)
                             (> (string-length set-subtitle) 0))
                        set-subtitle
                        (tag-ref tags 'albumtitle))])
    (string-append (truncate-file-str (sluggify-str meta-name) 60)
                   "."
                   (path->extension path))))

;; get-album-identity : (listof is-file?) -> hash?
(define (get-album-identity files)
  (let ([audio-files (filter audio-file? files)])
    (if (null? audio-files)
        (error! "No audio files found in directory.")
        (get-audio-tags (car audio-files)))))

;; multi-disc? : hash? -> bool?
(define (multi-disc? tags)
  (let* ([total (tag-try-get tags 'disctotal)])
    (and total
         (let ([n (string->number total)])
           (and n (> n 1))))))

;; make-base-dirpath : hash? (listof string?) bool? -> string?
(define (make-base-dirpath tags files shared?)
  (let* ([album-artist (sluggify-str (tag-ref tags 'albumartist))]
         [album-title
          (truncate-file-str (sluggify-str (tag-ref tags 'albumtitle)) 60)]
         [recording-date (substring (tag-ref tags 'recordingdate) 0 4)]
         [file-ext (string->upper (path->extension (car (filter audio-file? files))))]
         [media-type (let ([raw (tag-ref tags 'originalmediatype)])
                       (cond
                         [(equal? raw "Digital Media") "WEB"]
                         [(string-contains? raw "CD") "CD"]
                         [else raw]))]
         [catalog-num-raw (tag-try-get tags 'catalognumber)]
         [catalog-num (if (list? catalog-num-raw)
                          (car catalog-num-raw)
                          catalog-num-raw)]
         [album-folder-name
          (string-append album-title
                         " "
                         (format "(~a) " recording-date)
                         (format "[~a ~a]" file-ext media-type)
                         (if catalog-num (format " {~a}" catalog-num) ""))]
         [cd-folder (if (multi-disc? tags)
                        (string-append "CD" (pad-to-width (tag-ref tags 'discnumber) 2))
                        #f)])
    (if shared?
        (string-join (filter string?
                             `(,(format "~a - ~a" album-artist album-folder-name) ,cd-folder))
                     "/")
        (string-join (filter string?
                             `("/mnt" "EXTREME_SSD" "Music" ,album-artist ,album-folder-name ,cd-folder))
                     "/"))))

;; organize-directory : is-dir? -> void?
(define (organize-directory path shared?)
  (let* ([entries (read-dir path)]
         [audio-files (filter audio-file? entries)])
    (cond
      ;; case: album or disc
      [(null? audio-files)
       (for-each (λ (dir) (organize-directory dir shared?))
                 (filter is-dir? entries))]
      [else
        (let* ([tags (get-audio-tags (car audio-files))]
               [target-dir (make-base-dirpath tags audio-files shared?)])
          (unless (path-exists? target-dir)
                  (create-directory! target-dir))
          (for-each (λ (entry)
                       (let* ([base-name (file-name entry)]
                              [new-name (cond [(audio-file? entry) (get-new-filename entry)]
                                              [(meta-file? entry) (get-new-meta-filename entry tags)]
                                              [else base-name])]
                              [dest (string-append target-dir "/" new-name)]
                              [current-dir (canonicalize-path (parent-name entry))])
                         (unless (and (equal? current-dir (canonicalize-path target-dir))
                                      (equal? new-name base-name))
                                 (log-move! entry dest)
                                 (rename! entry dest))))
                    entries))])
    (delete-if-empty! path)))

;; log-move! : string? string? -> void?
(define (log-move! src dest)
  (displayln (format "┌─ Source: ~a" src))
  (displayln (format "└─ Target: ~a" dest))
  (displayln ""))

;; tag-ref : (hashof string? (or/c string? (listof string?))) symbol? -> string?
(define (tag-ref tags key)
  (hash-ref tags (symbol->string key)))

;; tag-try-get : hash? symbol? -> (string? | #f)
(define (tag-try-get tags key)
  (hash-try-get tags (symbol->string key)))

;; display-tags! : hash? -> void?
(define (display-tags! tags)
  (for-each (λ (kv)
       (displayln `(,(car kv) ,(cdr kv))))
    (hash->list tags)))

;;; CLI

(define (cli/handler ctx)
  (displayln ctx)
  (let ([shared? (hash-try-get ctx "--shared")]
        [interactive? (hash-try-get ctx "--interactive")]
        [dirs (hash-try-get ctx 'args)])
    (unless dirs
      (error! "Provide at least 1 argument."))
    (for-each (lambda (dir)
                (displayln (format "Organizing: ~a" dir))
                (organize-directory dir shared?))
              dirs)))

(make-command rename_music.scm
  (doc "Reorganize music files")
  (options
    (shared "-s" "--shared" "Use shared naming scheme" (flag #t))
    (interactive "-i" "--interactive" "Enable interactive mode" (flag #t)))
  (subcommands)
  (positionals)
  (handler cli/handler))

(define (main)
  (let ([args (drop (command-line) 3)])
    (parse-args rename_music.scm args)))

(main)
