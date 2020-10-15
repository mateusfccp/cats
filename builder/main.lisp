;;; Core

(defun main ()
  (with-open-file (in-stream "template.md" :if-does-not-exist :error)
    (let ((template-string (make-string (file-length in-stream))))
      (read-sequence template-string in-stream)
      ;; (str-replace template-string "{{meet}}" (generate-table (get-cats-names)))
      (with-open-file (out-stream "../README.md" :direction :output :if-does-not-exist :create :if-exists :overwrite)
        (write-string (str-replace template-string "{{meet}}" (generate-table (get-cats-names))) out-stream)))))
        
(defun get-cats-names ()
  "Return a list of the cats names."
  (let* ((directory (uiop:subdirectories "../cats"))
         (folders (map 'list #'get-folder-name directory)))
    folders))

(defun get-cat-pictures (cat-name)
  (remove-if #'is-not-image (uiop:directory-files (str-replace "../cats/$dir/" "$dir" cat-name))))

(defun get-cat-profile-picture (cat-name)
  (let* ((files (get-cat-pictures cat-name))
         (first-file (file-namestring (first files))))
    (str-replace (str-replace "./cats/$name/$file" "$name" cat-name) "$file" first-file)))

(defun get-folder-name (pathname)
  "Return only the pathname's folder as string instead of the entire path."
  (first (last (pathname-directory pathname))))

;; Generators

(defun generate-table (cats-names)
  (str-replace "<table>$1</table>" "$1" (generate-table-rows cats-names)))

(defun generate-table-cell (cat-name)
  (str-replace (str-replace "<td>$1$2</td>" "$1" (generate-img (get-cat-profile-picture cat-name))) "$2" cat-name))

(defun generate-table-rows (cats-names)
  (let* ((current-names (subseq cats-names 0 (min (length cats-names) 3)))
         (cells (map 'list #'generate-table-cell current-names))
         (cells-string (apply #'concatenate 'string cells))
         (row-string (str-replace "<tr>$1</tr>" "$1" cells-string)))
    (if (null (cdddr cats-names))
        row-string
        (concatenate 'string row-string (generate-table-rows (cdddr cats-names))))))
  
(defun generate-cats-mds ()
  (loop for cat-name in (get-cats-names)
        do (generate-single-md cat-name)))

(defun generate-single-md (cat-name)
  "Generate a markdown file for the given cat-name."
  (let* ((files (get-cat-pictures cat-name) )
         (files-names (map 'list #'file-namestring files)))
    (with-open-file (out-stream (concatenate 'string "../cats/" cat-name "/README.md") :direction :output :if-does-not-exist :create :if-exists :overwrite)
      (write-line (concatenate 'string "# Meet " cat-name) out-stream)
      (loop for file-name in files-names
            do (write-line (generate-img file-name) out-stream)))))

(defun generate-img (url)
  (str-replace "<img src=\"$url\" width=\"250\">" "$url" url))

;;; Utils

(defun str-replace (string substring replacement)
  "Returns a new string with substring replaced with replacement."
  (let ((needle-position (search substring string)))
    (if (null needle-position)
        string
        (let* ((needle-length (length substring))
               (before (subseq string 0 needle-position))
               (after (subseq string (+ needle-position needle-length))))
          (concatenate 'string before replacement after)))))

(defconstant image-extensions '("jpg" "jpeg" "bmp" "png" "gif"))

(defun is-not-image (pathname)
  (not (is-image pathname)))

(defun is-image (pathname)
  (let* ((comparator (lambda (extension) (string= extension (pathname-type pathname))))
        (compared-list (map 'list comparator  image-extensions)))
    (reduce (lambda (a b) (or a b)) compared-list)))
