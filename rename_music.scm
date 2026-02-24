#!/usr/bin/env steel

(#%require-dylib "libsteel_audio_tags"
                 (only-in extract-audio-tags))

(require "steel/iterators")
(require "steel/result")
(require "srfi/srfi-28/format.scm")

(define g-supported-audio-file-types '("flac" "mp3"))
(define g-supported-meta-file-types '("cue" "log"))

;; pad-2 : string? -> string?
(define (pad-2 s)
  (if (= (string-length s) 1)
      (string-append "0" s)
      s))

;; sluggify-str : string? -> string?
(define (sluggify-str s)
  (let* ([blacklist '(#\/ #\\ #\* #\: #\? #\; #\| #\< #\>)]
         [chars (string->list s)]
         [replaced-chars (map (λ (c)
                                (if (member c blacklist)
                                    #\_
                                    c))
                              chars)])
    (list->string replaced-chars)))

;; truncate-file-str : string? number? -> string?
(define (truncate-file-str s max-len)
  (if (> (string-length s) max-len)
      (string-append (substring s 0 (- max-len 3)) "...")
      s))

;; get-new-filename : string? -> string?
(define (get-new-filename path)
  (let* ([ext (path->extension path)]
         [tags (extract-audio-tags path)]
         [disc-number (pad-2 (or (tag-value tags 'discnumber) 1))]
         [track-number (pad-2 (get-req-tag tags 'tracknumber))]
         [track-title (truncate-file-str (sluggify-str (get-req-tag tags 'tracktitle))
                                         60)])
    (format "~a.~a - ~a.~a" disc-number track-number track-title ext)))

;; get-album-identity : (listof string?) -> hash-table?
(define (get-album-identity files)
  (let ([audio-files (filter is-audio? files)])
    (if (null? audio-files)
        (error! "No audio files found in directory.")
        (extract-audio-tags (car audio-files)))))

;; multi-disc? : hash-table? -> bool?
(define (multi-disc? tags)
  (let ([total (tag-value tags 'disctotal)])
    (and total 
         (let ([n (string->number total)])
           (and n (> n 1))))))

;; make-base-dirpath : hash-table? (listof string?) -> void?
(define (make-base-dirpath tags files)
  (let* ([album-artist (sluggify-str (get-req-tag tags 'albumartist))]
         [album-title (sluggify-str (get-req-tag tags 'albumtitle))]
         [recording-date (substring (get-req-tag tags 'recordingdate) 0 4)]
         [file-ext (string->upper (path->extension (first (filter is-audio? files))))]
         [media-type (let ([raw (get-req-tag tags 'originalmediatype)])
                       (if (equal? raw "Digital Media") "WEB" raw))]
         [catalog-num (tag-value tags 'catalognumber)]
         [album-folder-name (string-append album-title " "
					   (format "(~a) " recording-date)
					   (format "[~a ~a]" file-ext media-type)
					   (if catalog-num (format " {~a}" catalog-num) ""))]
         [cd-folder (if (multi-disc? tags)
                        (string-append "CD" (pad-2 (get-req-tag tags 'discnumber)))
                        #f)])
    (string-join (filter string?
                         `("/mnt" "EXTREME_SSD" "Music" ,album-artist ,album-folder-name ,(or cd-folder #f)))
                 "/")))

					; ;; make-base-dirpath-shared : hash-table? (listof string?) -> void?
					; (define (make-base-dirpath-shared tags files)
					;   (let* ([album-artist (sluggify-str (get-req-tag tags 'albumartist))]
					;          [album-title (sluggify-str (get-req-tag tags 'albumtitle))]
					;          [recording-date (substring (get-req-tag tags 'recordingdate) 0 4)]
					;          [file-ext (string->upper (path->extension (first (filter is-audio? files))))]
					;          [media-type (let ([raw (get-req-tag tags 'originalmediatype)])
					;                        (if (equal? raw "Digital Media") "WEB" raw))]
					;          [catalog-num (tag-value tags 'catalognumber)]
					;          [album-folder-name (string-append album-artist " - "
					;                                            album-title " "
					;                                            (format "(~a) " recording-date)
					;                                            (format "[~a ~a]" file-ext media-type)
					;                                            (if catalog-num (format " {~a}" catalog-num) ""))]
					;          [cd-folder (if (multi-disc? tags)
					;                         (string-append "CD" (pad-2 (get-req-tag tags 'discnumber)))
					;                         #f)])
					;     (string-join (filter string? `(,album-folder-name ,(or cd-folder #f)))
					;                  "/")))

;; is-audio? : string? -> bool?
(define (is-audio? path)
  (and (is-file? path)
       (member (path->extension path)
               g-supported-audio-file-types)))

;; is-meta? : string? -> bool?
(define (is-meta? path)
  (and (is-file? path)
       (member (path->extension path)
               g-supported-meta-file-types)))

;; get-req-tag : hash-table? symbol? -> string?
(define (get-req-tag tags key)
  (or (tag-value tags key)
      (error! (format "Field not found: ~a" key))))

;; organize-directory : string? -> void?
(define (organize-directory path)
  (let* ([entries (read-dir path)]
         [full-paths (map canonicalize-path entries)]
         [audio-files (filter is-audio? entries)])
    (cond
     ;; case: album or disc
     [(not (null? audio-files))
      (let* ([tags (extract-audio-tags (car audio-files))]
             [target-dir (make-base-dirpath tags audio-files)])
        (unless (path-exists? target-dir)
          (create-directory! target-dir))
        (for-each (λ (entry)
                    (let* ([base-name (file-name entry)]
                           [new-name (cond [(is-audio? entry)
                                            (get-new-filename entry)]
                                           [(is-meta? entry)
                                            (string-append (sluggify-str (get-req-tag tags 'albumtitle))
                                                           "." (path->extension entry))]
                                           [else base-name])]
                           [dest (string-append target-dir "/" new-name)]
                           [current-dir (canonicalize-path (parent-name entry))])
                      (unless (and (equal? current-dir
                                           (canonicalize-path target-dir))
                                   (equal? new-name base-name))
                        (spawn-process (command "mv" `("-v" ,entry ,dest))))))
                  full-paths))]
     [else
      (for-each (λ (dir)
                  (when (is-dir? dir)
                    (organize-directory dir)))
                full-paths)])))

;; tag-value : hash-table? symbol? -> (string? | #f)
(define (tag-value tags key)
  (hash-try-get tags (symbol->string key)))

;; display-tags : hash-table? -> void?
(define (display-tags tags)
  (for-each (λ (kv)
              (displayln (list (car kv) (cdr kv))))
            (hash->list tags)))

;; main : (listof any?) -> void?
(define (main args)
  (cond
   [(or (empty? args)
        (> (length args) 1)
        (not (is-dir? (car args))))
    (error! "Usage: rename_music.scm <directory>")]
   [else
    (organize-directory (car args))
    (displayln "Organization complete.")]))

(main (list-tail (command-line) 2))
