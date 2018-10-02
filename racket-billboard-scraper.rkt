 #lang racket
(require csc151)
(require html-parsing)
(require net/url)

;;; File Name
;;;   racket-billboard-scraper.rkt
;;; Authors
;;;   Won Seok Chung
;;;   Tran Le
;;;   Jonathan Gomez
;;; Summary
;;;   The program scrapes Billboard's current top 100/200 songs and converts into a CSV file.
;;; Citations
;;;   Grinnell College CS151 Racket Library: https://github.com/grinnell-cs/csc151
;;;   Racket html-parsing Library: https://docs.racket-lang.org/html-parsing/index.html
;;;   Billboard's Hot 100: https://www.billboard.com/charts/hot-100
;;;   Billboard's 200: https://www.billboard.com/charts/billboard-200
;;;   CSC151 Readings: http://www.cs.grinnell.edu/~rebelsky/Courses/CSC151/2018S/home/schedule



;;; ----- GETTING STARTED --------------------------------------------------------------

;;; URL of the Billboard Website (i.e. https://www.billboard.com/charts/hot-100,
;;;                                    https://www.billboard.com/charts/billboard-200)
;(display  "                     <How to Run the Program>")(newline)
;(display "  1. Edit the 'save-to' above to set the folder for CSV file to be saved.") (newline)
;(display "  2. Run the program") (newline)
;(display "  3. On Interactions, either type print-table to print the table or save-table to
;                                save the table as a CSV file in the directed folder.")
(newline)(newline)(newline)
(display "  ----------------------------------- START -----------------------------------") (newline)(newline)


(display    "    Hey! We are the Cutie Racket Billboard Scaper! <3") (newline) (newline)
        

(define charts
  (list "https://www.billboard.com/charts/hot-100"
        "https://www.billboard.com/charts/billboard-200"
        "https://www.billboard.com/charts/hot-holiday-songs"
        "https://www.billboard.com/charts/country-songs"
        "https://www.billboard.com/charts/dance-electronic-songs"
        "https://www.billboard.com/charts/billboard-korea-k-pop-100"))

(display "    Here is the list of Billboard charts:") (newline)
(newline)
(display "        1. The Hot 100") (newline)
(display "        2. Billboard 200") (newline)
(display "        3. Holiday 100") (newline)
(display "        4. Top Country Songs") (newline)
(display "        5. Top Dance/Electronic songs") (newline)
(display "        6. Billboard K-pop 100") (newline)
(newline)
(display "  Please enter the index of the chart you want to see:") (newline)
(define val (read)) 
(define get-url (list-ref charts (- val 1)))

(display    "    What would you like to do?") (newline) (newline)
(display    "       1. Print the chart") (newline)
(display    "       2. Save to a csv file") (newline)
(display    "       3. Print & Save") (newline)
(display    "       4. Quit") (newline)







;;; ----- LIST OF GENERAL PROCEDURES --------------------------------------------------------------


;;; Procedure:
;;;   page-get
;;; Parameters:
;;;   url, a string
;;; Purpose:
;;;   Parses the HTML documentation of the webpage of given URL
;;; Produces:
;;;;  List of heterogeneous lists (HTML Documentation of webpage)
;;; Preconditions:
;;;   url should be a valid webpage address
;;; Postconditions:
;;;   [No additional]

(define page-get
  (lambda (url)
    (call/input-url (string->url url) get-pure-port html->xexp)))


;;; Procedure:
;;;   tag?
;;; Parameters:
;;;   lst, a list
;;;   tag, any type
;;; Purpose:
;;;   Check whether any of the element lists begin with tag.
;;; Produces:
;;;   present?, a Boolean value
;;; Preconditions:
;;;   [No Addition]
;;; Postconditions:
;;;   * Return #t if any entry has the first element equal to tag.
;;;   * Return #f if no entry has the first element equal to tag.
;;;   * Does not affect the table.

(define tag?
  (lambda (lst tag)
    (let ([lst (filter list? lst)])
      (cond [(assoc tag lst) #t]
            [else #f]))))

;;; Procedure:
;;;   take-table
;;; Parameters:
;;;   lst, a list
;;;   tag, any type
;;; Purpose:
;;;   return only entries that begine with tag.
;;; Produces:
;;;   result, a list of lists
;;; Preconditions:
;;;   * Each entry in directory must be a list.
;;; Postconditions:
;;;   * result is a list contains all entries from lst that have tag as the first element.
;;;   * Does not affect lst.
;;;   * If no entries contain tags as the first element, result is a null list. 

(define take-table
  (lambda (lst tag)
    (let kernel ([so-far null]
                 [remaining lst])
      (cond [(null? remaining)
             so-far]
            [(equal? tag (caar remaining))
             (kernel (cons (car remaining) so-far) (cdr remaining))]
            [else (kernel so-far (cdr remaining))]))))


;;; Procedure:
;;;   filter-table
;;; Parameters:
;;;   lst, a table of sort
;;;   tag, any type
;;; Purpose:
;;;   work with table of multiple layers, reduce table to the right level and
;;;        take entries that have tag as the first element. 
;;; Produces:
;;;   result, a list of lists.
;;; Preconditions:
;;;   * Entries must be lists.
;;;   * There must be at least a list element in one level that contains tag
;;;           as the first element. 
;;; Postconditions:
;;;   * result is a list contains all entries from lst that have tag as the first element.
;;;   * The desired entries are on the second level.
;;;   * Does not affect lst.

(define filter-table
  (lambda (lst tag)
    (cond [(tag? lst tag)
           (take-table (filter list? lst) tag)]
          [else (filter-table
                 (reduce append (filter list? (filter (negate null?) lst))) tag)])))


;;; Procedure:
;;;   leave-table
;;; Parameters:
;;;   lst, a table of sort
;;;   tag, any type
;;; Purpose:
;;;   work with table of multiple layers, reduce table to the right level and
;;;        take entries that have tag as the first element. 
;;; Produces:
;;;   result, a list of lists.
;;; Preconditions:
;;;   * Entries must be lists.
;;;   * There must be at least a list element in one level that contains tag
;;;           as the first element. 
;;; Postconditions:
;;;   * result is a list contains all entries from lst
;;;             that are one the same level as those have tag as the first element.
;;;   * The desired entries are on the second level.
;;;   * Does not affect lst.

(define leave-table
  (lambda (lst tag)
    (cond [(tag? lst tag)
           (filter list? lst)]
          [else (leave-table
                 (reduce append (filter list? (filter (negate null?) lst))) tag)])))


;;; Procedure:
;;;   convert-table
;;; Parameters:
;;;   table, a list of lists
;;;   source, an output port. 
;;; Purpose:
;;;   create a csv file with input as an association list
;;; Produces:
;;;   Nothing. 
;;; Preconditions:
;;;   There is a file by the given name.
;;;   It is possible to write to the standard output port. 
;;; Postconditions:
;;;   Does not affect the file.
;;;   File contains all data from the input table.   
;;;   File retains the structure from the input table.

(define convert-table
  (lambda (table source)
    (letrec ([convert-csv
              (lambda (lst)
                (cond [(null? (cdr lst))
                       (write (car lst) source)
                       (newline source)]
                      [else
                       (write (car lst) source)
                       (display "," source)
                       (convert-csv (cdr lst))]))])
      (cond [(null? table)
             (close-output-port source)]
            [(convert-csv (car table))
             (convert-table (cdr table) source)]))))





;;; Procedure:
;;;   rid-of
;;; Parameters:
;;;   str, a string
;;; Purpose:
;;;   Removes the last character of a string, which will be used to remove newline tag ("\n")
;;; Produces:
;;;   clean-string, a string
;;; Preconditions:
;;;   (length str) >= 1
;;; Postconditions:
;;;   (+ 1 (length clean-string)) = (length str)
;;;   clean-string has same characters as str except it doesn't have the last string

(define rid-of
  (lambda (str)
    (substring str 0 (- (string-length str) 1))))


;;; ----- LIST OF SIMPLE OPERATIONS & PROJECT-SPECIFIC PROCEDURES --------------------------------------------------------------


;;; HTML Documentation of the Webpage
(define page-html (page-get get-url))

;;; Filtered HTML documentation of Webpage with <article> tags (Relevant documentation)
(define filtered (filter-table page-html 'article))

;;; Filtered documentation with <h2> tags
(define h2 (leave-table filtered 'h2))

;;; Filtered documentaiton with <span> tags
(define span (filter-table filtered 'span))

;;; Procedure:
;;;   song-title
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Create a list of lists of song titles
;;; Produces:
;;;   title-list, a list of lists of a string
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   [No additional]

(define song-title
  (map list (map (section list-ref <> 2) (filter-table filtered 'h2))))


;;; Procedure:
;;;   filter-artist
;;; Parameters:
;;;   lst, a list of lists
;;; Purpose:
;;;   Filters out and retruns a list of lists with <a> and <span> tags
;;; Produces:
;;;   lists, a list of lists
;;; Preconditions:
;;;   (length str) >= 1
;;; Postconditions:
;;;   (+ 1 (length clean-string)) = (length str)
;;;   clean-string has same characters as str except it doesn't have the last string

(define filter-artist
  (lambda (lst)
    (cond
      [(null? lst) null]
      ;; Checks for <a> tag
      [(equal? ((o car car) lst) 'a)
       (cons (car lst) (filter-artist (cdr lst)))]
      ;; Checks for <span> tag
      [(equal? ((o car car) lst) 'span)
       (cons (car lst) (filter-artist (cdr lst)))]
      [else
       (filter-artist (cdr lst))])))


;;; Procedure:
;;;   artist-name
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Create a list of lists of artist names
;;; Produces:
;;;   artist-list, a list of lists of a string
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   [No additional]

(define artist-name
  (map list (map rid-of (map (section list-ref <> 3)
                             (reverse (filter-artist h2))))))


;;; Procedure:
;;;   nums
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Create a list of numerical values for other rankings
;;; Produces:
;;;   title-list, a list of lists with '(@ (class "chart-row__value") as its second element.
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If the song got on chart this week, it will be marked as "--"

(define nums
  (map (section caddr <>)
       (filter (o (section equal? <> '(@ (class "chart-row__value")))
                  cadr) span)))


;;; Procedure:
;;;   three
;;; Parameters:
;;;   lst, a list of lists
;;; Purpose:
;;;   Create a list of lists for group of three consecutive strings
;;; Produces:
;;;   title-list, a list of lists for group of three consecutive strings
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If the song got on chart this week, it will be marked as "--"
;;;   Each element of title-list will have strings of numeric values or "--"

(define three
  (lambda (lst)
    (cond
      [(null? lst) null]
      [else (cons (take lst 3)
                  (three (drop lst 3)))])))


;;; Procedure:
;;;   other-rank
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Create a list of lists of other rankings
;;; Produces:
;;;   title-list, a list of lists of numbers
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If the song got on chart this week, the third element of element will be marked as "--"
;;;   Each element of title-list will have strings of numeric values or "--"

(define other-rank (three nums))


;;; Creates a list of lists with from 1 to 100
(define current-rank (map list (drop (iota (+ 1 (length artist-name))) 1)))

;;; Creates the top row of tables with column names
(define toprow '("Current Rank" "Song Title" "Artist" "Weeks on Chart" "Peak Position" "Last Week Rank"))



;;; ----- LIST OF PROGRAM FINAL COMMANDS --------------------------------------------------------------


;;; Prints the final table
(define print-table
  (cons toprow (map append current-rank song-title artist-name other-rank)))

;;; File Export Address (Change the name of the file and adresss of the folder to be saved)
;(define save-to "/Users/tranle/Desktop/practice.csv")
;;; Defines a source to output the table to (Can be updated)
;(define sample (open-output-file save-to #:exists 'can-update))
;;; Saves the final table to a folder
;(define save-table (convert-table print-table sample))


(define run-program
  (let ([val (read)])
    (cond [(= 1 val)
           (display "   Here is the Table") (newline)
           print-table]
          [(= 2 val)
           (display "What name would you like to save it as?
                     (Letter, Number, Whitespace, and Hyphen only)")(newline)
           (let* ([save-to (let
                               ([command (symbol->string (read))]
                                [pwd-string (path->string (current-directory))])
                             (string-append pwd-string command ".csv"))]
                  [sample (open-output-file save-to #:exists 'can-update)])
             (display (string-append "\n\n The following table has been saved at " save-to)) (newline)
             (convert-table print-table sample))]
          [(= 3 val)
           (display "What name would you like to save it as?
                      (Letter, Number, Whitespace, and Hyphen only) ")(newline)
           (let* ([save-to (let
                               ([command (symbol->string (read))]
                                [pwd-string (path->string (current-directory))])
                             (string-append pwd-string command ".csv"))]
                  [sample (open-output-file save-to #:exists 'can-update)])
             (convert-table print-table sample)
             (display (string-append "\n\n The following table has been saved at " save-to)) (newline)
             (display "   Here is the Table") (newline)
             print-table
             )]
          [else (display "Awwww! We are sad that you quit. Bye Bye then! ^^")])))
run-program
