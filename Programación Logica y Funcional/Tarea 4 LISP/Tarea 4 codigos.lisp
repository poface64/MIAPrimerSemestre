
;; 1.- Implemente en Lisp las siguientes operaciones sobre conjuntos
;; representados como listas. [15 puntos]

;; Subconjunto
(defun subset (a b &key (test #'eql))
  "Devuelve T si A es subconjunto de B, NIL en caso contrario.
   El parámetro opcional TEST permite personalizar la comparación."
  (every (lambda (element)
           (find element b :test test))
         a))

(subset '(1 3) '(1 2 3 4))
(subset '( ) '(1 2 3 4))


;; 1.2 Intersección

(defun inter (conjunto1 conjunto2)
  "Devuelve la intersección de dos conjuntos representados como listas."
  (remove-duplicates
   (remove-if-not (lambda (x) (member x conjunto2))
                  conjunto1)))

(inter '(1 2 3) '(2 3 4))

;; 1.3 Union
(defun union1 (conjunto1 conjunto2)
  "Devuelve la unión de dos conjuntos representados como listas."
  (remove-duplicates (append conjunto1 conjunto2)))

(union1 '(1 2 3 4) '(2 3 4 5))

;; 1.4 Diferencia
;; Capitulo 3 del libro de commond lisp

(defun dif (conjunto1 conjunto2)
  "Devuelve la diferencia de dos conjuntos representados como listas."
  (remove-if (lambda (x) (member x conjunto2)) conjunto1))

(dif '(1 2 3 4) '(2 3 4 5))

(dif '(1 2 3) '(1 4 5))

;; 2. Escriba un programa en que elimine todas las ocurrencias de un elemento en una lista.


(defun eliminar (elemento lista)
  "Elimina todas las ocurrencias de ELEMENTO en LISTA sin usar funciones predefinidas."
  (cond
    ((null lista) nil) ; Caso base: si la lista es vacia
    ((eql (car lista) elemento) ; Si el primer elemento coincide, eliminar 
     (eliminar elemento (cdr lista))) ; Continuar con el resto de la lista.
    (t ; Caso contrario, conservar el elemento y seguir recorriendo
     (cons (car lista) (eliminar elemento (cdr lista))))))

(eliminar 3 '(1 3 2 4 5 3 6 7))

;; '3. Implemente una función en Lisp que dada una lista de átomos, regresa las
posibles permutaciones de sus miembros.

(defun perms (lista)
  "Devuelve una lista con todas las permutaciones posibles de LISTA."
  (if (null lista)
      '(()) ; Caso base: la permutación de una lista vacía es una lista vacía.
      (mapcan
       (lambda (x)
         (mapcar (lambda (perm)
                   (cons x perm))
                 (perms (remove x lista :count 1))))
       lista)))
  
(perms '(1 2 3))

;; 5.- Lea el capítulo 22 (LOOP for Black Belts) del libro de Peter Seibel, 
;; Practical Common Lisp. Utilice la macro loop para resolver alguno de los 
;; ejercicios propuestos en esta tarea.

(defun eliminarlp (elemento lista)
  "Elimina todas las ocurrencias de ELEMENTO en LISTA usando LOOP."
  (loop for x in lista
        unless (eql x elemento) ; Si `x` no es igual a `elemento`, inclúyelo.
        collect x))

(eliminarlp 3 '(1 3 2 4 5 3 6 7))

;; 6.- Defina una macro repeat que tenga el siguiente comportamiento. 
;; Evidentemente, la expresión que se repite puede ser cualquier 
;; expresión válida en Lisp. [15 puntos]:

(defmacro repeat (n &rest body)
  "Repite la expresión BODY N veces."
  (let ((i (gensym)))  ; Genera un nombre único para el índice
    `(dotimes (,i ,n)   ; Repite el cuerpo N veces
       ,@body)))         ; Expansión del cuerpo (se ejecuta en cada iteración)
(repeat 5 (print "Hola, mundo!"))

;; 7. La siguiente función me permite definir una entrada en un registro de mis
;; libros:

; Función para crear un registro de libro

(defun crea-libro (titulo autor ed precio)
  (list :titulo titulo :autor autor :ed ed :precio precio))

; Definir la variable global para almacenar las entradas

(defvar *db* nil)

; Definir la función que agrega libros a la base de datos db
(defun agregar-reg (libro) (push libro *db*))

;Agregue más entradas al registro 

(agregar-reg (crea-libro "El Aleph" "Jorge Luis Borges" "Debolsillo" 199.00))
(agregar-reg (crea-libro "Ficciones"  "Jorge Luis Borges" "Debolsillo" 259.00))
(agregar-reg (crea-libro "El libro de los sueños" "Jorge Luis Borges" "Debolsillo" 229.00))
(agregar-reg (crea-libro "Análisis de algoritmos" "Homero Vladimir Ríos Figueroa" "UV"  20.00))
(agregar-reg (crea-libro "Pericia Artificial" "Alejandro Guerra Hernández" "UV"  90.50))

; Definir la función para que se proyecte bonito
(defun dump-db ()
  (dolist (libro *db*)
    (format t "~{~a:~10t~a~%~}~%" libro)))

(dump-db)

; 8.- Defina una función para recuperar una entrada en el registro buscando por autor

(defun buscar-por-autor (autor)
  "Busca y devuelve una lista de libros escritos por el autor dado."
  (remove-if-not 
    (lambda (libro)
      (equal (getf libro :autor) autor))
                 *db*))

;; Ejemplo de uso:
(let ((resultado (buscar-por-autor "Jorge Luis Borges")))
  (dolist (libro resultado)
    (format t "~{~a:~10t~a~%~}~%" libro)))

(let ((resultado (buscar-por-autor "Alejandro Guerra Hernández")))
  (dolist (libro resultado)
    (format t "~{~a:~10t~a~%~}~%" libro)))
