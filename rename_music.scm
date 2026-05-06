#!/usr/bin/env -S steel --

(#%require-dylib "libsteel_taglib" (only-in get-audio-tags
                                            get-audio-properties
                                            rename-file!))

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
      (format "~a..." (substring s 0 (- len 3)))
      s))

;; get-new-filename : audio-file? -> string?
;; Make new file name for the specified audio file.
(define (get-new-filename path)
  (let* ([ext (path->extension path)]
         [tags (get-audio-tags path)]
         [disc-number (pad-to-width (or (meta-try-get tags 'discnumber) 1) 2)]
         [track-number (pad-to-width (meta-ref tags 'tracknumber) 2)]
         [track-title
          (truncate-file-str (sluggify-str (meta-ref tags 'tracktitle)) 60)])
    (format "~a.~a - ~a.~a" disc-number track-number track-title ext)))

;; get-new-meta-filename : string? -> string?
;; Make new file name for the specified metadata file.
(define (get-new-meta-filename path tags)
  (let* ([set-subtitle (meta-try-get tags 'setsubtitle)]
         [meta-name (if (and (string? set-subtitle)
                             (> (string-length set-subtitle) 0))
                        set-subtitle
                        (meta-ref tags 'albumtitle))])
    (format "~a.~a"
            (truncate-file-str (sluggify-str meta-name) 60)
            (path->extension path))))

;; get-album-identity : (listof is-file?) -> hash?
(define (get-album-identity files)
  (let ([audio-files (filter audio-file? files)])
    (if (null? audio-files)
        (error! "No audio files found in directory.")
        (get-audio-tags (car audio-files)))))

;; multi-disc? : hash? -> bool?
(define (multi-disc? tags)
  (let* ([total (meta-try-get tags 'disctotal)])
    (and total
         (let ([n (string->number total)])
           (and n (> n 1))))))

;; make-base-dirpath : hash? hash? (listof string?) bool? -> string?
(define (make-base-dirpath tags props files shared?)
  (let* ([album-artist (sluggify-str (meta-ref tags 'albumartist))]
         [album-title (truncate-file-str (sluggify-str (meta-ref tags 'albumtitle)) 60)]
         [recording-date (substring (meta-ref tags 'recordingdate) 0 4)]
         [file-ext (string->upper (path->extension (car (filter audio-file? files))))]
         [media-type
           (let ([raw (meta-ref tags 'originalmediatype)])
             (cond
               [(equal? raw "Digital Media") "WEB"]
               [(string-contains? raw "CD") "CD"]
               [else raw]))]
         [catalog-num
           (let ([raw (meta-try-get tags 'catalognumber)])
             (if (list? raw) (car raw) raw))]
         [bit-stat
           (case file-ext
                 [("FLAC")
                  (let ([bd (meta-try-get props 'bit-depth)])
                    (and bd (> bd 16) (number->string bd)))]
                 [("MP3")
                  (let ([br (meta-try-get props 'overall-bitrate)])
                    (displayln br)
                    (and br (if (< br 320) "V0" "320")))]
                 [else #f])]
         [album-folder-name (string-join
                              (filter string?
                                      `(,(and shared? (format "~a -" album-artist))
                                        ,(format "~a (~a) [~a"
                                                 album-title
                                                 recording-date
                                                 file-ext)
                                        ,(and shared? bit-stat)
                                        ,(format "~a]" media-type)
                                        ,(and catalog-num (format "{~a}" catalog-num))))
                              " ")]
         [cd-folder (and (multi-disc? tags)
                         (format "CD~a" (pad-to-width (meta-ref tags 'discnumber) 2)))])
    (string-join (filter string?
                         `(@,(if shared? '() (list "/mnt" "exfat" "Music" album-artist))
                            ,album-folder-name ,cd-folder))
                 "/")))

;; organize-directory : is-dir? -> void?
(define (organize-directory path shared?)
  (let* ([dirents (read-dir path)]
         [audio-files (filter audio-file? dirents)])
    (cond
      ;; case: album or disc
      [(null? audio-files)
       (for-each (λ (dir) (organize-directory dir shared?))
                 (filter is-dir? dirents))]
      [else
        (let* ([tags (get-audio-tags (car audio-files))]
               [props (get-audio-properties (car audio-files))]
               [target-dir (make-base-dirpath tags props audio-files shared?)])
          (unless (path-exists? target-dir)
                  (create-directory! target-dir))
          (for-each (λ (dirent)
                       (let* ([base-name (file-name dirent)]
                              [new-name (cond [(audio-file? dirent) (get-new-filename dirent)]
                                              [(meta-file? dirent) (get-new-meta-filename dirent tags)]
                                              [else base-name])]
                              [dst (string-append target-dir "/" new-name)]
                              [current-dir (canonicalize-path (parent-name dirent))])
                         (unless (and (equal? current-dir (canonicalize-path target-dir))
                                      (equal? new-name base-name))
                                 (log-move! dirent dst)
                                 (rename-file! dirent dst))))
                    dirents))])
    (delete-if-empty! path)))

;; log-move! : string? string? -> void?
(define (log-move! src dst)
  (displayln (format "┌─ Source: ~a" src))
  (displayln (format "└─ Target: ~a" dst))
  (displayln ""))

;; meta-ref : (hashof string? (or/c string? (listof string?))) symbol? -> string?
(define (meta-ref tags key)
  (hash-ref tags (symbol->string key)))

;; meta-try-get : hash? symbol? -> (string? | #f)
(define (meta-try-get tags key)
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
