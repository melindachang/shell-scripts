#!/usr/bin/env -S sbcl --script

;;;; Script to move and rename music files.

(require :uiop)

(load "~/.sbclrc")

(ql:quickload "clingon" :silent t)
(ql:quickload "taglib" :silent t)

(defvar +supported-audio-file-types+ '("flac" "mp3"))
(defvar +supported-meta-file-types+ '("cue" "log"))

(defstruct rename-action
  (type :file) ; :file | :cleanup
  src
  dst)

;;; UTILS

(defun trim-null-chars (str)
  (string-right-trim '(#\Null #\Space) str))

(defun sluggify (str &optional (max-len 60))
  (when str
    (let* ((bad-chars '(#\/ #\\ #\* #\: #\? #\| #\< #\>))
           (clean (map 'string (lambda (c) (if (member c bad-chars) #\_ c)) str)))
      (if (> (length clean) max-len)
          (concatenate 'string (subseq clean 0 (- max-len 3)) "...")
          clean))))

(defun get-parsed-int (val)
  "Parse integer from string, or return if already is one."
  (typecase val
    (integer val)
    (string (parse-integer val :junk-allowed t))
    (t nil)))

(defun normalize-media (raw)
  (when raw
    (cond ((string-equal raw "Digital Media") "WEB")
          ((search "CD" raw) "CD")
          (t raw))))

(defun make-filename (tags)
  "Generate new file name string based on TAGS."
  (let ((disc (getf tags :disc-number))
        (track (getf tags :track-number))
        (title (sluggify (getf tags :title) 60)))
    (format nil "~2,'0d.~2,'0d - ~A" disc track title)))

(defun make-album-dirname (tags ext &key (format :default))
  "Generate new directory name based on TAGS and FORMAT."
  (let ((album          (sluggify (getf tags :album) 60))
        (album-artist   (sluggify (getf tags :album-artist) 60))
        (year           (getf tags :date))
        (media          (getf tags :media))
        (catalog-number (getf tags :catalog-number))
        (ext-up         (string-upcase ext)))
    (case format
      (:default
       (format nil "~A (~A) [~A ~A]~@[ {~A}~]"
               album-artist album year ext-up media catalog-number))
      (:shared
       (let ((bit-label (cond
                          ((string-equal ext-up "MP3")
                           (let ((br (getf tags :bit-rate)))
                             (if (and br (>= br 320000)) "320" "V0")))
                          ((string-equal ext-up "FLAC")
                           (let ((bd (getf tags :bit-depth)))
                             (if (and bd (/= bd 16)) bd nil))))))
         (format nil "~A - ~A (~A) [~A ~@[~A ~]~A]~@[ {~A}~]"
                 album-artist album year ext-up bit-label media catalog-number))))))


(defun resolve-disc-subfolder (tags)
  "Returns disc subfolder name, or NIL if none is needed."
  (let ((total-discs (or (getf tags :total-discs) 1))
        (disc-number (or (getf tags :disc-number) 1)))
    (when (> total-discs 1)
      (format nil "CD~2,'0d" disc-number))))


(defun plan-rename (src-dir dst-base-dir &key (format :default))
  (let ((actions nil)
        (entries (uiop:directory-files src-dir))
        (subdirs (uiop:subdirectories src-dir)))
    (let ((audio-files
            (remove-if-not (lambda (f)
                             (member (pathname-type f)
                                     +supported-audio-file-types+
                                     :test #'string-equal))
                           entries)))
      (if (null audio-files)
          (dolist (dir subdirs)
            (setf actions (append actions (plan-rename dir dst-base-dir :format format))))
          (let* ((album-tags (audio-file-tags (first audio-files)))
                 (album-artist (sluggify (getf album-tags :album-artist)))
                 (album-dir (make-album-dirname album-tags (pathname-type (first audio-files)) :format format))
                 (base-dst (uiop:merge-pathnames* (if (eq format :shared)
                                                      (make-pathname :directory `(:relative ,album-dir))
                                                      (make-pathname :directory `(:relative ,album-artist ,album-dir)))
                                                  dst-base-dir)))
            (dolist (entry entries)
              (let* ((ext (pathname-type entry))
                     (is-audio-p (member ext +supported-audio-file-types+ :test #'string-equal))
                     (is-meta-p (member ext +supported-meta-file-types+ :test #'string-equal))
                     (tags (when is-audio-p (audio-file-tags entry)))
                     (new-name
                       (cond (is-audio-p (make-filename tags))
                             (is-meta-p
                              (sluggify (getf album-tags :album)))
                             (t
                              (pathname-name entry))))
                     (new-ext
                       (if (or is-audio-p is-meta-p)
                           (string-downcase ext)
                           (pathname-type entry)))
                     (subfolder (and is-audio-p
                                     (resolve-disc-subfolder tags)))
                     (dst (uiop:merge-pathnames* (make-pathname :name new-name
                                                                :type new-ext
                                                                :directory (if subfolder `(:relative ,subfolder) nil))
                                                 base-dst)))
                (unless (equal entry dst)
                  (push (make-rename-action :src entry
                                            :dst dst)
                        actions))))
            (push (make-rename-action :type :cleanup :src src-dir) actions))))
    actions))

(defun execute-plan (plan &key dry-run)
  (dolist (action (nreverse plan))
    (ecase (rename-action-type action)
      (:file
       (let ((src (rename-action-src action))
             (dst (rename-action-dst action)))
         (format t "┌─ Source: ~A~%└─ Target: ~A~%~%" src dst)
         (unless dry-run
           (ensure-directories-exist dst)
           (uiop:rename-file-overwriting-target src dst))))
      (:cleanup
       (let ((dir (rename-action-src action)))
         (unless dry-run
           (when (null (uiop:directory-files dir))
             (format t "Cleaning up empty directory: ~A~%" dir)
             (uiop:delete-empty-directory dir))))))))

;;; TAGLIB PARSING

(defun id3-get-tag (file frame)
  (let ((frames (id3:get-frames file `(,frame))))
    (when frames
      (when (> (length frames) 1)
        (warn "Multiple ~a tags found, using first occurrence" frame))
      (let ((val (id3:info (first frames))))
        (when (stringp val)
          (trim-null-chars val))))))

(defun tag-present-p (value)
  "Returns T if value is not NIL and not an empty string."
  (and value
       (not (and (stringp value)
                 (string= (string-trim " " value) "")))))

(defgeneric %parse-audio-tags (file-type path)
  (:documentation "Parse audio tags from file at PATH based on FILE-TYPE."))

(defvar +tag-defaults+ '(:disc-number 1 :total-discs 1))
(defvar +required-tags+ '(:title :album :album-artist :track-number :disc-number :total-discs :date :media))

(defmethod %parse-audio-tags :around (file-type path)
  (let ((tags (call-next-method)))
    (loop for (key default-value) on +tag-defaults+ by #'cddr
          do (unless (getf tags key)
               (setf (getf tags key) default-value)))

    (setf (getf tags :track-number) (get-parsed-int (getf tags :track-number)))
    (setf (getf tags :disc-number) (get-parsed-int (getf tags :disc-number)))
    (setf (getf tags :total-discs) (get-parsed-int (getf tags :total-discs)))
    (setf (getf tags :date) (subseq (getf tags :date) 0 4))
    (setf (getf tags :media) (normalize-media (getf tags :media)))

    (dolist (req-key +required-tags+)
      (unless (tag-present-p (getf tags req-key))
        (error "Missing required tag '~A' in file: ~A" req-key path)))

    tags))

(defmethod %parse-audio-tags (file-type path)
  (declare (ignore file-type path))
  (error "Unsupported file type: ~A" file-type))

(defmethod %parse-audio-tags ((file-type (eql :flac)) path)
  (let* ((file (or (audio-streams:open-audio-file path)
                   (error "Failed to parse file: ~A" path)))
         (tags (flac:flac-tags file))
         (info (flac:audio-info file)))
    (list :title          (flac:flac-get-tag tags "title")
          :album          (flac:flac-get-tag tags "album")
          :album-artist   (flac:flac-get-tag tags "albumartist")
          :track-number   (flac:flac-get-tag tags "tracknumber")
          :disc-number    (flac:flac-get-tag tags "discnumber")
          :total-discs    (flac:flac-get-tag tags "totaldiscs")
          :date           (flac:flac-get-tag tags "date")
          :media          (flac:flac-get-tag tags "media")
          :catalog-number (flac:flac-get-tag tags "catalognumber")
          :bit-depth      (flac::bits-per-sample info)
          :sample-rate    (flac::sample-rate info))))

(defmethod %parse-audio-tags ((file-type (eql :mp3)) path)
  (let* ((file (or (audio-streams:open-audio-file path)
                   (error "Failed to parse file: ~A" path)))
         (info (id3:audio-info file))
         (txxx-index (let ((index (make-hash-table :test #'equalp))
                           (frames (id3:get-frames file '("TXXX"))))
                       (dolist (frame frames)
                         (setf (gethash (id3:desc frame) index)
                               (trim-null-chars (id3:val frame))))
                       index)))
    (list :title           (id3-get-tag file "TIT2")
          :album           (id3-get-tag file "TALB")
          :album-artist    (id3-get-tag file "TPE2")
          :track-number    (id3-get-tag file "TRCK")
          :disc-number     (id3-get-tag file "TPOS")
          :total-discs     (gethash "TOTALDISCS" txxx-index)
          :date            (id3-get-tag file "TDRC")
          :media           (id3-get-tag file "TMED")
          :catalog-number  (gethash "CATALOGNUMBER" txxx-index)
          :bit-rate        (mpeg::bit-rate info))))

(defun audio-file-tags (path)
  "Returns plist of parsed audio tags from file at path."
  (unless (probe-file path)
    (error "File does not exist: ~A" path))
  (let ((file-ext (pathname-type path)))
    (unless (member file-ext +supported-audio-file-types+ :test #'string-equal)
      (error "Unsupported file type: '~A'" file-ext))
    (let ((type-keyword (intern (string-upcase file-ext) "KEYWORD")))
      (%parse-audio-tags type-keyword path))))

;;; CLINGON DEFS

(defun cli/options ()
  "Defines CLI options for the `rename-music' command"
  `(,(clingon:make-option
      :flag
      :description "Print actions without committing"
      :long-name "dry-run"
      :short-name #\d
      :key :dry-run)
    ,(clingon:make-option
      :flag
      :description "Use shared naming scheme"
      :long-name "shared"
      :short-name #\s
      :key :shared)
    ,(clingon:make-option
      :flag
      :description "Enable interactive mode"
      :long-name "interactive"
      :short-name #\i
      :key :interactive)))

(defun cli/handler (cmd)
  "Handler for the `rename-music' command"
  (let* ((dry-run     (clingon:getopt cmd :dry-run))
         (shared      (clingon:getopt cmd :shared))
         (interactive (clingon:getopt cmd :interactive))
         (args        (clingon:command-arguments cmd))
         (src-dir     (first args)))
    (unless src-dir
      (format t "Error: Missing source directory argument.~%")
      (clingon:exit 1))

    (let* ((src-path (uiop:ensure-directory-pathname
                      (uiop:parse-native-namestring src-dir)))
           (format-type (if shared :shared :default))
           (dst-base-dir (if shared
                             (uiop:getcwd)
                             (uiop:ensure-directory-pathname "/mnt/EXTREME_SSD/Music/"))))
      (format t "Planning rename for ~A...~%" src-dir)

      (let ((plan (plan-rename src-path dst-base-dir :format format-type)))
        (if plan
            (execute-plan plan :dry-run dry-run)
            (format t "Nothing to rename.~%"))))))

(defun cli/command ()
  (let ((clingon:*default-options*
          `(,clingon:*default-version-flag*
            ,clingon:*default-bash-completions-flag*
            ,(clingon:make-option
              :flag
              :description "Display usage information and exit"
              :long-name "help"
              :short-name #\h
              :key :clingon.help.flag))))
    (clingon:make-command
     :name "rename-music"
     :description "Move & rename music files."
     :version "0.1.0"
     :authors '("Melinda Chang <melindachang@proton.me>")
     :license "GPL-3.0"
     :options (cli/options)
     :handler #'cli/handler)))

(defun main ()
  (clingon:run (cli/command)))

(main)
