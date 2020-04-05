;;;     To execute, merely load, reset and run.
;;;======================================================

;;****************
;;* DEFFUNCTIONS *
;;****************
(deftemplate pokemon
    (slot nombre)
    (slot tipo)
    (slot clase)
    (slot categoria)
)

(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question si no Si No SI NO S N s n))
   (if (or (eq ?response si) (eq ?response Si) (eq ?response S) (eq ?response s))
       then TRUE 
       else FALSE))

;;;***************
;;;* QUERY RULES *
;;;***************
(defrule eliminar-tipos
    (tipo-definido ?deftipo)
    ?id <- (pokemon (tipo ?tipo))
    =>
    (if (not(eq ?deftipo ?tipo))
        then (retract ?id)
    )
)

(defrule determinar-tipo-planta ""
    (not (tipo-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es de tipo planta? (Si/No)?")
    then 
    (assert (tipo-definido planta))) 
)

(defrule determinar-tipo-fuego ""
    (not (tipo-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es de tipo fuego? (Si/No)?")
    then (assert (tipo-definido fuego)))
)

(defrule determinar-tipo-agua ""
    (not (tipo-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es de tipo agua? (Si/No)?")
    then (assert (tipo-definido agua)))
)

(defrule eliminar-clases
    (clase-definido ?defclase)
    ?id <- (pokemon (clase ?clase))
    =>
    (if (not(eq ?defclase ?clase))
        then (retract ?id)
    )
)

(defrule determinar-clase-anfibio ""
    (not (clase-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es un anfibio? (Si/No)?")
    then (assert (clase-definido anfibio)))
)

(defrule determinar-clase-ave ""
    (not (clase-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es un ave? (Si/No)?")
    then (assert (clase-definido ave)))
)

(defrule determinar-clase-mamifero ""
    (not (clase-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es un mamifero? (Si/No)?")
    then (assert (clase-definido mamifero)))
)

(defrule determinar-clase-pez ""
    (not (clase-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es un pez? (Si/No)?")
    then (assert (clase-definido pez)))
)

(defrule determinar-clase-reptil ""
    (not (clase-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es un reptil? (Si/No)?")
    then (assert (clase-definido reptil)))
)


(defrule eliminar-categorias
    (categoria-definido ?defcategoria)
    ?id <- (pokemon (categoria ?categoria))
    =>
    (if (not(eq ?defcategoria ?categoria))
        then (retract ?id)
    )
)

(defrule determinar-categoria-sapo ""
    (not (categoria-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es un sapo? (Si/No)?")
    then (assert (categoria-definido sapo)))
)

(defrule determinar-categoria-lagartija ""
    (not (categoria-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es una lagartija? (Si/No)?")
    then (assert (categoria-definido lagartija)))
)

(defrule determinar-categoria-tortuga ""
    (not (categoria-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es una tortuga? (Si/No)?")
    then (assert (categoria-definido tortuga)))
)

(defrule determinar-categoria-dinosaurio ""
    (not (categoria-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es un dinosaurio? (Si/No)?")
    then (assert (categoria-definido dinosaurio)))
)

(defrule determinar-categoria-erizo ""
    (not (categoria-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es un erizo? (Si/No)?")
    then (assert (categoria-definido erizo)))
)

(defrule determinar-categoria-cocodrilo ""
    (not (categoria-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es un cocodrilo? (Si/No)?")
    then (assert (categoria-definido cocodrilo)))
)

(defrule determinar-categoria-rana ""
    (not (categoria-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es una rana? (Si/No)?")
    then (assert (categoria-definido rana)))
)

(defrule determinar-categoria-pollo ""
    (not (categoria-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es un pollo? (Si/No)?")
    then (assert (categoria-definido pollo)))
)

(defrule determinar-categoria-pez ""
    (not (categoria-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es un pez? (Si/No)?")
    then (assert (categoria-definido pez)))
)

(defrule determinar-categoria-mono ""
    (not (categoria-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es un mono? (Si/No)?")
    then (assert (categoria-definido mono)))
)

(defrule determinar-categoria-pinguino ""
    (not (categoria-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es un pinguino? (Si/No)?")
    then (assert (categoria-definido pinguino)))
)

(defrule determinar-categoria-serpiente ""
    (not (categoria-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es una serpiente? (Si/No)?")
    then (assert (categoria-definido serpiente)))
)

(defrule determinar-categoria-cerdo ""
    (not (categoria-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es un cerdo? (Si/No)?")
    then (assert (categoria-definido cerdo)))
)

(defrule determinar-categoria-nutria ""
    (not (categoria-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es una nutria? (Si/No)?")
    then (assert (categoria-definido nutria)))
)

(defrule determinar-categoria-zorro ""
    (not (categoria-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es un zorro? (Si/No)?")
    then (assert (categoria-definido zorro)))
)

(defrule determinar-categoria-buho ""
    (not (categoria-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es un buho? (Si/No)?")
    then (assert (categoria-definido buho)))
)

(defrule determinar-categoria-gato ""
    (not (categoria-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es un gato? (Si/No)?")
    then (assert (categoria-definido gato)))
)

(defrule determinar-categoria-foca ""
    (not (categoria-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es una foca? (Si/No)?")
    then (assert (categoria-definido foca)))
)

(defrule determinar-categoria-conejo ""
    (not (categoria-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es un conejo? (Si/No)?")
    then (assert (categoria-definido conejo)))
)

(defrule determinar-categoria-camaleon ""
    (not (categoria-definido ?))
    (not (terminado ?))
    =>
    (if (yes-or-no-p "Es un camaleon? (Si/No)?")
    then (assert (categoria-definido camaleon)))
)


;;;********************************
;;;* STARTUP AND CONCLUSION RULES *
;;;********************************

(defrule system-banner ""
    (declare (salience 30))
    =>
    (printout t crlf crlf)
    (printout t "Adivinador de iniciales")
    (printout t crlf crlf)
    (load-facts pokemons.clp)
)

(defrule system-out ""
    (tipo-definido ?)
    (clase-definido ?)
    (categoria-definido ?)
    (pokemon 
        (nombre ?nombre)
    )
    =>
    (printout t crlf crlf)
    (printout t ?nombre)
    (printout t crlf crlf)
)