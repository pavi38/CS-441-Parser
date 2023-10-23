#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))    
(require data/either)
(require data/monad)
(require data/functor)

(define (either-chain f E)
  (if (failure? E)
      E  
      (f (from-success 0 E))))

;variable for counting the line number

(define n 1)

;funcation for making token from input string

(define scanner
         
         (lexer
         ["while" (cons `(while, n,"while")(scanner input-port))]
         ["endwhile" (cons `(endwhile, n,"endwhile")(scanner input-port))]
         ["read" (cons `(read,n,"read")(scanner input-port))]
         ["write" (cons `(write,n,"write")(scanner input-port))]
         ["if" (cons `(if,n,"if")(scanner input-port))]
         ["goto" (cons `(goto,n,"goto")(scanner input-port))]
         ["gosub" (cons `(gosub,n,"gosub")(scanner input-port))]
         ["return" (cons `(return,n,"return")(scanner input-port))]
         ["break" (cons `(break,n,"break")(scanner input-port))]
         ["end" (cons `(end,n,"end")(scanner input-port))]
         ["true" (cons `(TRUE,n,"true")(scanner input-port))]
         ["false" (cons `(FALSE,n,"false")(scanner input-port))]
         ["=" (cons `(ASSIG,n,"=")(scanner input-port))]
         ["$$" (cons `(dollar,n)(scanner input-port))]
         [(:or "+" "-" "*" "/" "*")(cons `(OP ,n,(string->symbol lexeme))(scanner input-port))]
         [(:or "<" ">" ">=" "<>" "<=")(cons `(BOOLOP ,n,(string->symbol lexeme))(scanner input-port))] ;test
         [(::(:or (char-range #\a #\z) (char-range #\A #\Z))(:* (:or (char-range #\a #\z) (char-range #\A #\Z)(char-range #\0 #\9)))) (cons `(ID ,n,lexeme)(scanner input-port))]
         [(repetition 1 +inf.0
                (char-range #\0 #\9))(cons `(Digit ,n,lexeme)(scanner input-port))]
         
         [#\: (cons `(colon,n,"colon")(scanner input-port))]
         [#\; (cons `(SEMI,n,"semicolon")(scanner input-port))]
         [#\( (cons `(LPAR,n,"left pranthese")(scanner input-port))]
         [#\) (cons `(RPAR,n,"right pranthese")(scanner input-port))]
         [#\newline   (begin (set! n (add1 n))(scanner input-port))]
         [whitespace (scanner input-port)]
         [(eof) '()]
         ))


;PARSER FUNCATIONS START FROM HERE.


;funcation for the NUMSIGN production.
;numsign -> + | - | epsilon

(define (num-sign token-list)
  (cond [(equal? (third(first token-list)) '(+))(success (rest token-list))]
        [(equal? (third(first token-list)) '(-)) (success  (rest token-list))]
        [else (success  token-list)]))

;Fucation for the NUM non-terminal.
;num -> numsign digit digit*

(define (num token-list)
  (cond
        [(empty? token-list) (failure '(n))]
        [(equal? (car(first token-list)) 'Digit)  (success  (rest token-list))]
        [else ((failure (cdr(first token-list))))]
        ))

;Fucation for the etail non-terminal.
;etail -> + expr | - expr | * expr | / expr | epsilon

(define (etail token-list)
  (if (equal? (car(first token-list)) 'OP)(expr (rest token-list))(success token-list)))

;Fucation for the expr non-terminal.
;expr -> id etail | num etail | (expr)

(define (expr token-list)
  (cond
        [(empty? token-list) (failure '(n))]
        [(equal? (car(first token-list)) 'ID) (etail (rest token-list))]
        [(equal? (car(first token-list)) 'LPAR)(chain close-prathenses(expr (rest token-list)))]
        [ success? (chain etail (chain num (num-sign token-list)))]
        [else ((failure (cdr(first token-list))))]))

;bool-op production
;bool-op -> < | > | >= | <= | <> | =

(define (bool-op token-list)
  (if(or(equal? (car(first token-list)) 'BOOLOP)(equal? (car(first token-list)) 'ASSIG))(success (rest token-list))((failure (cdr(first token-list))))))

;boolean production
;boolean -> true | false | expr bool-op expr

(define (boolean token-list)
  (cond[(or (equal? (car(first token-list)) 'TRUE)(equal? (car(first token-list)) 'FALSE)) (success (rest token-list))]
       [else (chain expr (chain bool-op(expr token-list)))]
       ))

;return success if semicolon found else failure

(define (semicolon token-list)
  (if (equal? (car(first token-list)) 'SEMI) (success ( rest token-list))(failure (cdr(first token-list)))))

(define (open-prathenses token-list)
  (if (equal? (car(first token-list)) 'LPAR)(success (rest token-list))(failure (cdr(first token-list)))))

(define (close-prathenses token-list)
  (if (equal? (car(first token-list)) 'RPAR)(success (rest token-list))(failure (cdr(first token-list)))))

(define (id token-list)
  (if (equal? (car(first token-list)) 'ID) (success (rest token-list))(failure (cdr(first token-list)))))

(define (endwhile token-list)
  (cond[(equal? (car(first token-list)) 'endwhile) (success (rest token-list))]
       [else (failure (cdr(first token-list)))] ;change 
       ))

;stmt production

(define (stmt token-list)
  (cond
        [(< (length token-list) 2) (failure '(n))] ;if list size less than 2 than we don't have enough entities to have a syntatically correct code
        [(and (equal? (car(first token-list)) 'ID) (equal? (car(second token-list)) 'ASSIG) )(chain semicolon (expr (rest ( rest token-list))))]
        [(equal? (car(first token-list)) 'if) (chain semicolon( chain stmt(chain close-prathenses(chain boolean(open-prathenses(rest token-list))))))]
        [(equal? (car(first token-list)) 'read) (chain semicolon(id (rest token-list)))]
        [(equal? (car(first token-list)) 'write)(chain semicolon(expr (rest token-list)))]
        [(equal? (car(first token-list)) 'goto)(chain semicolon(id (rest token-list)))]
        [(equal? (car(first token-list)) 'gosub)(chain semicolon(id (rest token-list)))]
        [(equal? (car(first token-list)) 'return) (semicolon (rest token-list))]
        [(equal? (car(first token-list)) 'break) (semicolon (rest token-list))]
        [(equal? (car(first token-list)) 'end) (semicolon (rest token-list))]
        [(equal? (car(first token-list)) 'while) (chain semicolon (chain endwhile (chain linelist(chain close-prathenses(chain boolean(open-prathenses (rest token-list)))))))]
        [else (failure (cdr(first token-list)))]
        ))

;linetail production
;linetail -> stmt+ | epsilon

(define (linetail token-list)
  (cond
    [(empty? token-list)(success token-list)]
    ;[(equal? (car(first token-list)) 'dollar) (begin(displayln 1)(success token-list))]
    [(success? (stmt token-list))(chain linetail(stmt token-list))]
    [else (success token-list)])
  )

(define (colon token-list)
  (if(equal? (car (first token-list)) 'colon)(success (rest token-list))(success token-list)))

;label production
;label -> id: | epsilon

(define (label token-list)
  (cond
       [(empty? token-list) (failure '(n))]
       [(and (equal? (car(first token-list)) 'ID) (equal? (car(second token-list)) 'colon))(success ( rest( rest token-list)))] 
       [else (success token-list)]
       ))

;line production
;line ->  label stmt linetail

(define (line token-list)
  ;(displayln (car(first token-list)))
  (chain linetail(chain stmt(label token-list))))

;linelist production
;linelist -> line linelist | epsilon

(define (linelist token-list)
  (cond
        [(empty? token-list)(failure '(n))]
        [(equal? (car(first token-list)) 'dollar) (success token-list)]
        [ (success? (chain linelist (line token-list)))(chain linelist (line token-list))]  ;[success? (chain linelist (line token-list))]
        [else (success token-list)]
        ))
;check if we have $$ sign at the end of the program
(define (dollar token-list)
  (if (equal? (car(first token-list)) 'dollar) (success ( rest token-list))(failure (cdr(first token-list)))))

(define (program token-list)
  (let([result (chain dollar(linelist token-list))])
    (cond
      [(success? result)(displayln "Accepted!!")]
      [else (displayln (~a "Syntax Error at line " (car(from-failure '() (result)))))] 
      ))
  )

(define (parser s)
  (cond [(string? s)(let([file-contents (open-input-file s)]) (parser (scanner file-contents)))] 
        [else (begin (set! n 1)(program s))]
        )  
)

