(defun main ()
  (with-open-file (in-stream "template.md" :if-does-not-exist :error)
    (let ((template-string (make-string (file-length in-stream))))
      (read-sequence template-string in-stream)
      (format t "~A" (str-replace template-string "{{meet}}" "meow!")))))

(defun str-replace (string substring replacement)
  "Returns a new string with substring replaced with replacement."
  (let ((needle-position (search substring string)))
    (if (null needle-position)
        string
        (let* ((needle-length (length substring))
               (before (subseq string 0 needle-position))
               (after (subseq string (+ needle-position needle-length))))
        (concatenate 'string before replacement after)))))
        
(defun get-cats-names ()
  "Return a list of the cats names."
  (let* ((directory (uiop:subdirectories "../cats"))
         (folders (map 'list #'get-folder-name directory)))
    folders))

(defun get-folder-name (pathname)
  "Return only the pathname's folder as string instead of the entire path."
  (first (last (pathname-directory pathname))))

(defun generate-single-md (cat-name)
  "Generate a markdown file for the given cat-name."
  (let* ((files (uiop:directory-files (concatenate 'string "../cats/" cat-name "/")))
         (files-names (remove (lambda (name) (string= name "README.md")) (map 'list #'file-namestring files))))
    (with-open-file (out-stream (concatenate 'string "../cats/" cat-name "/README.md") :direction :output :if-does-not-exist :create :if-exists :overwrite)
      (write-line (concatenate 'string "# Meet " cat-name) out-stream)
      (loop for file-name in files-names
            do (write-line (concatenate 'string "<img src=\"" file-name "\" width=\"250\">") out-stream)))))
