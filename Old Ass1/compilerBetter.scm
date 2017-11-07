(load "pc.scm")

#| —------------------------------------------------------------------- |#
#| —----------------------------Boolean-------------------------------— |#

(define <Boolean> 
  (new (*parser (char #\#))
    (*parser (char-ci #\f))
    (*caten 2)
    (*pack (lambda (_) 
      #f))
    (*parser (char #\#))
    (*parser (char-ci #\t))
    (*caten 2)
    (*pack (lambda (_) 
      #t))

    (*disj 2)
    done))

#| —------------------------------------------------------------------- |#
#| —------------------------------Char--------------------------------- |#

(define <CharPrefix> 
  (new 
    (*parser (char #\#))
    (*parser (char #\\))
  (*caten 2)
  done))

 (define <VisibleSimpleChar> 
  (new (*parser (range #\! #\~))

    done))
 
 (define ^<meta-char>
  (lambda (str ch)
    (new (*parser (word str))
   (*pack (lambda (_) ch))
   done)))

(define <NamedChar>
  (new
    (*parser (^<meta-char> "lambda" (integer->char 955)))
    (*parser (^<meta-char> "newline" #\newline))
    (*parser (^<meta-char> "space" #\space))
    (*parser (^<meta-char> "page" #\page))
    (*parser (^<meta-char> "return" #\return))
    (*parser (^<meta-char> "nul" #\nul))
    (*parser (^<meta-char> "tab" #\tab))
    (*disj 7)
    
    done))

(define <HexChar>
  (new
    (*parser (range #\0 #\9))
    (*parser (range-ci #\a #\f))
    (*disj 2)
  done))


(define <HexUnicodeChar>
  (new
    (*parser (char-ci #\x))
    (*parser <HexChar>)*plus
    (*pack (lambda (hex)(string->number
    		(list->string hex) 16)))
     (*guard (lambda (hex)
    (< hex 1114112)))
    (*caten 2)
    (*pack-with (lambda (x hex)(integer->char hex)))



 #|    (*pack (lambda (hex)(integer->char hex))) |#
    done))

(define <Char>
  (new
    (*parser <CharPrefix>)
    (*parser <NamedChar>)
    (*parser <HexUnicodeChar>)
    (*parser <VisibleSimpleChar>)
    (*disj 3)
    (*caten 2)
    (*pack-with (lambda(a b) b))  
    done))

#| —------------------------------------------------------------------- |#
#| —----------------------------Number--------------------------------- |#

(define <Natural>
  (new
    (*parser (range #\0 #\9)) *plus
    (*pack (lambda (num) (string->number
      (list->string `(,@num)))))
  done))

(define <Integer>
  (new
    (*parser (char #\+))
    (*parser <Natural>)
    (*caten 2)
    (*pack-with
      (lambda(a b) (+ b)))

    (*parser (char #\-))
    (*parser <Natural>)
    (*caten 2)
    (*pack-with
      (lambda(a b) (- b)))

    (*parser <Natural>)
    (*disj 3)

    done))

(define <Fraction>
  (new 
    (*parser <Integer>)
    (*parser (char #\/))
    (*parser <Natural>)
    (*guard (lambda (n) (not (zero? n))))
    (*caten 3)
    (*pack-with (lambda (int div nat)
      (/ int nat)))
    done))

(define <Number>
	(new 
         (*parser <Fraction>)
		 (*parser <Integer>)
		 (*delayed (lambda () <Symbol>))
		  *not-followed-by
		  (*disj 2)

	done))

#| —------------------------------------------------------------------- |#
#| —----------------------------String--------------------------------- |#

(define <StringLiteralChar>
   (new 
    (*parser <any-char>)
       (*parser (char #\\))
       (*parser (char #\"))
       (*disj 2)
       *diff
  done))


 (define <StringMetaChar>
   (new 
     (*parser (^<meta-char> "\\\\" #\\))
     (*parser (^<meta-char> "\\\"" #\"))
     (*parser (^<meta-char> "\\t" #\tab))
     (*parser (^<meta-char> "\\f" #\page))
     (*parser (^<meta-char> "\\n" #\newline))
     (*parser (^<meta-char> "\\r" #\return))
     (*parser (^<meta-char> "\\N" #\N))
        (*parser (^<meta-char> "\\R" #\R))
        (*parser (^<meta-char> "\\T" #\T))
        (*parser (^<meta-char> "\\F" #\F))  
     (*disj 10)
     done))
 
 (define <StringHexChar>
  (new
    (*parser (char #\\))
    (*parser (char-ci #\x))
    (*parser <HexChar>)*star
  
    (*pack (lambda (hex)(string->number
      (list->string `(,@hex))16)))

    (*guard (lambda (hex)
    	(< hex 1114112)))

    (*pack (lambda (hex)(integer->char hex)))
    (*parser (char #\;))
    (*caten 4)
    (*pack-with (lambda(a b c d) c))
    done))

(define <StringChar>
   (new 
     (*parser <StringMetaChar>)
     (*parser <StringLiteralChar>)
      (*parser <StringHexChar>)
      (*disj 3)
   done))

(define <String>
   (new
     (*parser (char #\"))
     (*parser <StringChar>)*star
     (*parser (char #\"))
     (*caten 3)
     (*pack-with(lambda(open-delim str end-delim)(list->string str)))
     done))

#| —------------------------------------------------------------------- |#
#| —----------------------------Symbol--------------------------------- |#

(define <SymbolChar>
  (new 
    (*parser (range #\0 #\9))
    (*parser (range-ci #\a #\z))
    (*pack (lambda(a) (if (< (char->integer a) 91)
             (integer->char (+ (char->integer a) 32)) a)))
    (*parser (char #\$))
    (*parser (char #\!))
    (*parser (char #\^))
    (*parser (char #\*))
    (*parser (char #\-))
    (*parser (char #\_))
    (*parser (char #\=))
    (*parser (char #\+))
    (*parser (char #\<))
    (*parser (char #\>))
    (*parser (char #\?))
    (*parser (char #\/))
    (*disj 14)
    done))

(define <Symbol>
  (new  
    (*parser <SymbolChar>)*plus
    (*pack (lambda (x) (string->symbol(list->string x))))
    done))

#| —------------------------------------------------------------------- |#
#| —----------------------------mayer addons--------------------------- |#

(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <line-comment>
  (let ((<end-of-line-comment>
   (new (*parser (char #\newline))
        (*parser <end-of-input>)
        (*disj 2)
        done)))
    (new (*parser (char #\;))
   
   (*parser <any-char>)
   (*parser <end-of-line-comment>)
   *diff *star

   (*parser <end-of-line-comment>)
   (*caten 3)
   done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <sexpr>))
       (*caten 2)
       done))

(define <infix-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <InfixExpression>))
       (*caten 2)
       done))

(define <comment>
  (disj <line-comment>
  		<infix-comment>
 		<sexpr-comment>))

(define <skip>
  (disj <comment>
  <whitespace>))

(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
     (*parser <p>)
     (*parser <wrapper>)
     (*caten 3)
     (*pack-with
      (lambda (_left e _right) e))
     done))))

(define ^<skipped*> (^^<wrapped> (star <skip>)))

;;;

(define add-list
  (lambda (s)
    (fold-right
     (lambda (a b) (+ a b))
     0
     s)))

#| —------------------------------------------------------------------- |#
#| —-------------------------Main function sexpr----------------------- |#
#| —------------------------------------------------------------------- |#

(define <sexpr>
  (^<skipped*>
    (new
    (*parser <Boolean>)
    (*parser <Number>)
    (*parser <Char>)
    (*parser <Symbol>)
    (*parser <String>)
    (*delayed (lambda()<ProperList>))
    (*delayed (lambda()<ImproperList>))
    (*delayed (lambda()<Vector>))
    (*delayed (lambda()<Quoted>))
    (*delayed (lambda()<QuasiQuoted>))
    (*delayed (lambda()<Unquoted>))
    (*delayed (lambda()<UnquoteAndSpliced>))
    (*delayed (lambda()<InfixExtension>))
    (*disj 13)
  done )))

#| —------------------------------------------------------------------- |#
#| —------------------------------------------------------------------- |#
#| —------------------------------------------------------------------- |#

#| —---------------------------ProperList----------------------------- |#

(define <ProperList> 
  (new
    (*parser (word "("))
    (*parser <sexpr>)*star
    (*parser (word ")"))
    (*caten 3)
    (*pack-with
        (lambda (_left e _right) e))
    done))

#| —------------------------------------------------------------------- |#

#| —----------------------------ImproperList----------------------------|#

(define <ImproperList> 
  (new
    (*parser (word "("))
    (*parser <sexpr>)*plus
    (*parser (word "."))
    (*parser <sexpr>)
    (*parser (word ")"))
    (*caten 5)
    (*pack-with
        (lambda (a b c d e) `(,@b . ,d)))
    done))

#| —------------------------------------------------------------------- |#
#| —----------------------------Vector--------------------------------- |#

(define <Vector> 
  (new
    (*parser (word "#"))
    (*parser (word "("))
    (*parser <sexpr>)*star
    (*parser (word ")"))
    (*caten 4)
    (*pack-with
        (lambda (x _left e _right) (list->vector `,e)))
    done))

#| —------------------------------------------------------------------- |#
#| —----------------------------Quoted--------------------------------- |#

(define <Quoted> 
  (new
    (*parser (word "'"))
    (*parser <sexpr>)
    (*caten 2)
    (*pack-with
        (lambda (_left e) `(,'quote ,e)))
    done))

#| —------------------------------------------------------------------- |#
#| —---------------------------QuasiQuoted----------------------------— |#

(define <QuasiQuoted> 
  (new
    (*parser (word "`"))
    (*parser <sexpr>)
    (*caten 2)
    (*pack-with
        (lambda (_left e) (list 'quasiquote e)))
    done))

#| —------------------------------------------------------------------- |#
#| —----------------------------Unquoted------------------------------- |#

(define <Unquoted> 
  (new
    (*parser (word ","))
    (*parser <sexpr>)
    (*caten 2)
    (*pack-with
        (lambda (_left e) (list 'unquote e)))
    done))

#| —------------------------------------------------------------------- |#
#| —--------------------------UnquoteAndSpliced------------------------ |#

(define <UnquoteAndSpliced> 
  (new
    (*parser (word ",@"))
    (*parser <sexpr>)
    (*caten 2)
    (*pack-with
        (lambda (a e) (list 'unquote-splicing e)))
    done))

#| —------------------------------------------------------------------- |#

#| —------------------------------------------------------------------- |#
#| —------------------------------------------------------------------- |#
#| —------------------------------------------------------------------- |#
#| —---------------------------Infix notation------------------------— |#
#| —------------------------------------------------------------------- |#
#| —------------------------------------------------------------------- |#
#| —------------------------------------------------------------------- |#

(define <InfixRoot> 
      (^<skipped*>
  (new

  (*delayed (lambda() <InfixParen>)) 
  (*delayed (lambda() <InfixNumber>))
  (*delayed (lambda() <InfixSymbol>))
  (*delayed (lambda() <InfixNeg>))
  (*delayed (lambda() <InfixSexprEscape> ))

  (*disj 5)

  done)))

#| —------------------------------------------------------------------- |#

(define <InfixPrefixExtensionPrefix> 
  (new
    (*parser (word "##"))
    (*parser (word "#%"))
    (*disj 2)
  done))

#| —------------------------------------------------------------------- |#

(define <InfixNeg>
  (^<skipped*>
  (new
    (*parser (word "-"))
    (*parser (char #\space))*star
    (*caten 2)
    (*pack-with(lambda(a b)(string->symbol "-")))
    
    (*delayed (lambda() <InfixArrayGet>))
    (*delayed (lambda()	<InfixFuncall>))
    (*delayed (lambda() <InfixNumber>))
    (*delayed (lambda() <InfixParen>))
    (*delayed (lambda() <InfixSymbol>))

     (*disj 5 )
    (*caten 2)

  done)))

#| —------------------------------------------------------------------- |#

(define <InfixExpression> 
	(^<skipped*>
  (new
    (*delayed (lambda()<Infix-Add-Sub>))
  done)))	

#| —------------------------------------------------------------------- |#

(define <InfixExtension> 
  (new
    (*parser <InfixPrefixExtensionPrefix>)
    (*parser <InfixExpression>)
    (*caten 2)
    (*pack-with
        (lambda (s e) e))
  done))

#| —------------------------------------------------------------------- |#
#| —-------------------------------Helpers----------------------------- |#

 (define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
        (else (list x))))

(define Exp-Build-Helper 
	(lambda(head rest)(fold-left 
		(lambda (x y) (list (car y) x (car `(,@(cdr y)))))
         (list (caar rest) head (car `(,@(cdar rest)))) (cdr rest))))

  (define PowHelper
 	 (lambda (x y)
 	 	(letrec ((pow (lambda(lst)
 	 		 (if  
 	 		 	(not(> 3 (length lst)))
 	 		  (list (caar lst) (car `(,@(cdar lst))) (pow (cdr lst)))
 	 		  (list (caar lst) (car `(,@(cdar lst))) (car `(,@(cdadr lst)
 	 	)))))))
 	 	 (if 
 	 	 	(> 2 (length y)) 
           (list (caar y) x (car `(,@(cdar y))))
           (list (caar y) x (pow y))

         ))))

#| —------------------------------------------------------------------- |#
#| —----------------------------Infix Add-Sub-------------------------- |#

(define <Infix-Add-Sub> 

  (new
    (*delayed (lambda() <Infix-Mul-Div>))
    (*parser (word "+"))
    (*pack (lambda(_)(string->symbol "+")))
    (*parser (word "-"))
    (*pack (lambda(_)(string->symbol "-")))
    (*disj 2)      
    (*delayed (lambda() <Infix-Mul-Div>))
    (*caten 2)*star
   
     (*caten 2)
    (*pack-with (lambda(a b) 
      (if 
      	(not (null? b)) 
        (Exp-Build-Helper a b)
		a )))
    done))

#| —------------------------------------------------------------------- |#
#| —----------------------------Infix Mul-Div-------------------------- |#

(define <Infix-Mul-Div> 
    (^<skipped*>
  (new
    (*delayed (lambda()  <InfixPow> ))

    (*parser (word "*"))
    (*pack (lambda(_)(string->symbol "*")))
    (*parser (word "/"))
    (*pack (lambda(_)(string->symbol "/")))
    (*disj 2)

    (*delayed (lambda()  <InfixPow> ))

(*caten 2)*star
    
     (*caten 2)
    (*pack-with (lambda(a b) 
      (if 
      	(not (null? b)) 
        (Exp-Build-Helper a b)
		a )))
    done)))

#| —------------------------------------------------------------------- |#

#| —----------------------------Infix Paren---------------------------- |#

(define <InfixParen>
  (^<skipped*> 
  (new
    (*parser (word "("))
    (*delayed (lambda()  <InfixExpression>))
    (*parser (word ")"))
    (*caten 3)
    (*pack-with
        (lambda (start exp end) exp))
    done)))

#| —------------------------------------------------------------------- |#
#| —---------------------------Infix Symbol---------------------------- |#

(define <InfixSymbol> 
  (new
  	(*parser (range #\0 #\9))
    (*parser (range-ci #\a #\z))
    (*pack (lambda(a) (if (< (char->integer a) 91)
     				(integer->char (+ (char->integer a) 32)) a)))
    (*parser (char #\$))
    (*parser (char #\!))
    (*parser (char #\_))
    (*parser (char #\=))
    (*parser (char #\<))
    (*parser (char #\>))
    (*parser (char #\?))

    (*disj 9)
	*plus	
	(*pack (lambda (sym) (string->symbol (list->string sym))))

    done))

(define <InfixNumber>
	(new 
         (*parser <Fraction>)
		 (*parser <Integer>)
		 (*parser <InfixSymbol>)
		 *not-followed-by
		 (*disj 2)
		
	done))

#| —------------------------------------------------------------------- |#
#| —--------------------------Infix ArrayGet--------------------------- |#

 (define <InfixArrayGet> 
    (^<skipped*>
  (new
    (*delayed (lambda() <InfixFuncall> ))

    (*parser (word "["))
    (*parser <Infix-Add-Sub>)
    (*delayed (lambda() <InfixFuncall> ))
    (*disj 2)
    (*parser (word "]"))
    (*caten 3)
    
    (*pack-with(lambda(a b c) `(vector-ref ,b)))
    *star
    
     (*caten 2)

    (*pack-with (lambda(head rest) 
      (if 
      	(not (null? rest)) 
        (Exp-Build-Helper head rest)
		head )))

	  done)))

 #| —------------------------------Infix Pow---------------------------- |#
 
 (define <InfixPow> 
    (^<skipped*>
  (new
    (*delayed (lambda() <InfixArrayGet> ))
    (*delayed (lambda() <PowerSymbol>))    
    (*delayed (lambda() <InfixArrayGet> ))

    (*caten 2)(*pack-with(lambda(a b) `(expt ,b)))
    *star
    
     (*caten 2)
     (*pack-with (lambda (x y)
               (if (not (null? y)) 
               	(PowHelper x y)
               	x))) 		 
    done)))

#| —------------------------------------------------------------------- |#
#| —---------------------------Power symbol---------------------------- |#

(define <PowerSymbol> 
    (^<skipped*>
  (new
    (*parser (word "^"))
    (*parser (word "**"))
    (*disj 2)
  done)))

#| —------------------------------------------------------------------- |#
#| —------------------------------------------------------------------- |#

 (define <InfixFuncall> 
 	 (^<skipped*>
	(new
		(*delayed (lambda() <InfixRoot>  ))

		(*parser (word "("))
		(*delayed (lambda() <InfixArgList>))	
		(*parser (word ")"))

		(*caten 3)
		(*pack-with(lambda(a b c) b))
		*plus
 		(*caten 2)

		(*pack-with (lambda (func args) (fold-left cons func args)))
		(*delayed (lambda() <InfixRoot> ))
		(*disj 2)

		done)))

#| --------------------------------------------------------------------- |#
#| -----------------------------Infix ArgList--------------------------- |#

 (define <InfixArgList> 
 	 (^<skipped*>
	(new
		(*parser <InfixExpression>)

		(*parser (word ","))
		(*parser <InfixExpression>)
		(*caten 2)
		(*pack-with(lambda(head rest) rest))
		*star

		(*caten 2)
		(*pack-with (lambda (head rest) (cons head rest)))

		(*parser <epsilon>)

		 (*disj 2)	
	done)))

#| --------------------------------------------------------------------- |#
#| -----------------------------Infix Escape---------------------------- |#
(define <InfixSexprEscape> 
  (new
    (*parser <InfixPrefixExtensionPrefix>)
    (*parser <sexpr>)
    (*caten 2)
    (*pack-with(lambda(a b) b))
  done))

#| --------------------------------------------------------------------- |#
#| --------------------------------------------------------------------- |#