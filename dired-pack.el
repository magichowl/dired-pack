;;;; dired-pack.el - extensions to dired to pack and unpack files.
;;;; Copyright (C) 1995 Jim Blandy

;; Author: Jim Blandy <jimb@cyclic.com>
;; Maintainer: Pan, Senshan <pansenshan@aliyun.com>
;; Created: Mon  6 Sep 1993
;; Updated: Tue 28 Apr 2015
;; Version: 1.8
;; Keywords: unix

;;; Commentary:

;;; dired-pack adds a command to dired-mode for packing and unpacking
;;; files.  When using this package, typing `T' on a file in a
;;; dired listing unpacks the file, unpacking it if necessary.
;;; Typing `T' on a file/directory packs up that file/directory into a gzipped
;;; tar file named DIRNAME.tgz.
;;;
;;; To use this package, just place it in a directory in your Emacs
;;; lisp load path, byte-compile it, and put the line
;;;    (require 'dired-pack)
;;; in your .emacs.
;;;
;;; This file defines the following function:
;;;
;;; dired-pack-unpack - If the file on the current line is a
;;;    packed file, unpack it.  If the
;;;    file on the current line is a file/directory, compress it.
;;;
;;; It also declares the following variables:
;;;
;;; dired-tar-should-gzip - if t, dired-pack gzips the tar files
;;;	it creates.  If nil, dired-pack leaves the tars alone.
;;;
;;; dired-tar-command-switches - flags to pass to the tar program.
;;;      This is concatenated with command characters ("x" or "c" or
;;;      whatever).  The default is 'vf'; I'm told Windows users
;;;      should use "mvf".
;;;
;;; dired-pack-extension - extension to use for gzipped tar files.
;;;      Defaults to ".tgz", but ".7z" may be a useful value in
;;;      some circumstances.
;;;
;;; dired-tar-gzip-command - a shell command which gzips its
;;;     standard input to its standard output.
;;;
;;; dired-tar-ungzip-command - a shell command which ungzips
;;;	its standard input to its standard output.
;;;
;;; dired-tar-shell-file-name - name of the shell to use to run the
;;;      tar command.  The default is /bin/sh, which should work for
;;;      all Unix users, better than your login shell; I'm told
;;;      Windows users may want to use "4NT", but I know nothing about
;;;      Windows.

;;; Changes since 1.6:
;;; - recognize files with extension .tgz as gzipped tarfiles; let user
;;;   configure what we name compressed files we create.
;;; Changes since 1.5:
;;; - (dired-pack): Changes from Cord Kielhorn: name files correctly
;;;   when dired-tar-should-gzip is false.
;;;
;;; Changes since 1.4:
;;; - added dired-tar-shell-file-name and dired-tar-command-switches;
;;;   thanks to Cristian Ionescu-Idbohrn <cii@kcs.se>!

;;; Code:

(require 'compile)


;;;; Variables.
(defvar dired-tar-should-gzip t
  "*t if dired-pack-unpack should gzip the files it creates, by default.
nil if it should leave them alone.")

(defvar dired-pack-extension ".tgz"
  "*File name extension to use for gzipped tar files.
By default, this is \".tgz\", but some people may like to use \".7z\".")

(defvar dired-tar-gzip-command "gzip --best --stdout"
  "*A shell command which gzips its standard input to its standard output.")

(defvar dired-pack-command "tar -czvf \"%s\" \"%s\""
  "*A shell command which gzips its standard input to its standard output.")

(defvar dired-tar-ungzip-command "gzip --decompress --stdout"
  "*A shell command which ungzips its standard input to its standard output.")

(defvar dired-tar-shell-file-name "/bin/sh"
  "The name of the shell to use to run the tar command.
The default is /bin/sh, which should work for all Unix users; I'm told
Windows users may want to use \"4NT\", but I know nothing about
Windows.")

(defvar dired-tar-command-switches "vf"
  "Flags to pass to the tar program, in addition to the command charcaters.
This is concatenated with command characters (\"x\" or \"c\" or
whatever).  The default is 'vf'; I'm told Windows users should use
\"mvf\".")

(defvar dired-pack-result nil
  "For internal use by dired-pack functions.
This variable is made local to the buffer in which we run the tar
process, and holds the name of the file created or affected.  The
process-termination sentinal uses this to update the dired listing
when the process completes its work, or dies.")

(defvar dired-pack-with-7za 'nil
  "Default compression function is set to \"tar\", if this variable is true, use \"7za\".")

;;;; Internal functions.

(defun dired-pack-run-command (command directory result)
  "Internal function for use by the dired-pack package.
Run COMMAND asynchronously in its own window, like a compilation.
Use DIRECTORY as the default directory for the command's execution.
RESULT is the name of the tar file which will be created, or the
name of the directory into which the tar file was unpacked."
  (let ((buf (dired-pack-get-buffer)))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (widen)
      (erase-buffer)
      (goto-char (point-min))
      (insert "cd " directory)
      (newline)
      (insert command)
      (newline)

      (setq buffer-read-only t
            mode-name "Tar-Output"
            default-directory directory)

      (set (make-local-variable 'dired-pack-result)
           result)
      (set (make-local-variable 'mode-line-process)
           '(": %s"))
      (set (make-local-variable 'compilation-finish-function)
           'dired-pack-operation-done)

      (let ((process
             ;; Chris Moore <Chris.Moore@src.bae.co.uk> says that the
             ;; tar commands barf using his version of the zsh.  We
             ;; don't need anything but the Bourne shell here; that's
             ;; the default value for dired-tar-shell-file-name.
             (let ((shell-file-name dired-tar-shell-file-name))
               (start-process-shell-command "*Tar*" buf command))))
        (set-process-sentinel process 'compilation-sentinel))
      (display-buffer buf))))

(defun dired-pack-get-buffer ()
  "Choose a buffer to run a tar process in.
Tar output buffers have names like *Tar*, *Tar*<2>, *Tar*<3>, ...
We return the lowest-numbered buffer that doesn't have a live tar
process in it.  We delete any other buffers whose processes have
deleted."

  ;; Kill all completed tar buffers.
  (let ((number 1))
    (while number
      (let* ((name (if (<= number 1) "*Tar*"
                     (format "*Tar*<%d>" number)))
             (buf (get-buffer name)))
        (if (null buf) (setq number nil)
          (save-excursion
            (set-buffer buf)
            (if (let ((process (get-buffer-process buf)))
                  (not (and process (eq (process-status process) 'run))))
                (kill-buffer buf)))
          (setq number (1+ number))))))

  ;; Make us a fresh buffer.
  (generate-new-buffer "*Tar*"))


(defun dired-pack-operation-done (buf message)
  "Internal function for use by the dired-pack package.
This function is run when the tar operation completes.  It tries to
update the dired listing by looking at dired-pack-result."
  (cond
   ((null dired-pack-result))

   ((file-directory-p dired-pack-result)
    (save-excursion
      (mapcar
       (function (lambda (buf)
                   (set-buffer buf)
                   (dired-revert)))
       (dired-buffers-for-dir dired-pack-result))))

   ((file-exists-p dired-pack-result)
    (dired-relist-file dired-pack-result))

   ;; Otherwise, I guess the tar operation must have failed somehow.
   ))

(defun dired-pack (directory prefix-arg)
  "Internal function for use by the dired-pack package.
Create a gzipped tar file from the contents of DIRECTORY.
The archive is named after the directory, and the files are stored in
the archive with names relative to DIRECTORY's parent.

We use `dired-pack-extension' as the suffix for the filenames we
create.

For example, (dired-pack \"/home/blandy/womble/\") would produce a
tar file named \"/home/blandy/womble.tar.gz\", whose contents had
names like \"womble/foo\", \"womble/bar\", etcetera.

The second argument PREFIX-ARG is ignored."
  (let* ((dir-file (directory-file-name directory))
         (tar-file-name (concat dir-file dired-pack-extension))
         (parent-name (file-name-directory dir-file))
         (content-name (file-name-nondirectory dir-file)))
    (dired-pack-run-command
     (format dired-pack-command
             tar-file-name
             content-name)
     parent-name
     tar-file-name)))

(defconst dired-pack-regexp
  (format "\\(%s$\\)\\'"
          (mapconcat 'regexp-quote
                     '(".tar" ".tar.z" ".tar.gz" ".tar.Z" ".tgz" ".tar.xz" ".txz" ".rar" ".zip" ".7z" ".tar.bz2")
                     "$\\|"))
  "Regular expression matching plausible filenames for tar files.")

(defconst dired-tar-gzipped-tarfile-regexp
  (format "\\(%s$\\)\\'"
          (mapconcat 'regexp-quote
                     '(".tar.z" ".tar.gz" ".tar.Z" ".tgz")
                     "$\\|"))
  "Regular expression matching plausible filenames for compressed files.")

(defun dired-unpack (tar-file prefix-arg)
  "Internal function for use by the dired-pack package.
Unpack TAR-FILE into the directory containing it.
If PREFIX-ARG is non-nil, just list the archive's contents without
unpacking it."

  (let ((tar-file-dir (file-name-directory tar-file))
        (action (if prefix-arg "t" "x")))
    (dired-pack-run-command
     (cond
      ;; Does it look like a compressed file?
      ((string-match dired-pack-regexp tar-file)
       (format "unar \"%s\"" tar-file))

      (t
       (error
        "bug: dired-unpack should only be passed tar file names.")))
     tar-file-dir

     ;; If we're just unpacking the archive, don't bother updating the
     ;; dired listing.
     (if prefix-arg nil tar-file-dir))))


;;;; User-visible functions.

;;;###autoload
(defun dired-toggle-pack-function ()
  (interactive)
  (setq dired-pack-with-7za (not dired-pack-with-7za))
  (if dired-pack-with-7za
      (setq dired-pack-extension ".7z"
            dired-pack-command "7za a -r \"%s\" \"%s\"")
    (if dired-tar-should-gzip
     (setq dired-pack-extension ".tgz"
           dired-pack-command "tar -czvf \"%s\" \"%s\"")
     (setq dired-pack-extension ".tar"
           dired-pack-command "tar -cvf \"%s\" \"%s\"")
     ))
  )

;;;###autoload
(defun dired-pack-unpack (prefix-arg)
  "Create or unpack a tar archive for the file on the current line.

If the file on the current line is a tar archive, unpack it.  If the
archive appears to be gzipped or compressed, expand it first.  With a
prefix argument, just list the tar archive's contents, and don't
unpack it.  The file's name must end in \".tar\", \".tar.gz\", or
\".tar.Z\" or else this command will assume it's not a tar file.

Otherwise, make a gzipped tar
file out of its contents.
"
  (interactive "P")

  (let ((filename (dired-get-filename)))
    (cond
     ((string-match dired-pack-regexp filename)
      (dired-unpack filename prefix-arg))

     (t
      (dired-pack filename prefix-arg)))))


;;;; Hooking this into dired mode.

;;;###autoload
(add-hook 'dired-mode-hook
          (function
           (lambda ()
             (define-key dired-mode-map "z" 'dired-pack-unpack)
             (define-key dired-mode-map "Z" 'dired-toggle-pack-function)
             )))


(provide 'dired-pack)

;;; dired-pack.el ends here
