;;;     To execute, merely load, reset and run.
;;;======================================================

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


(defrule eliminar-tipos-confirmado
    (tipo-definido ?deftipo)
    ?id <- (pokemon (tipo ?tipo))
    =>
    (if (not(eq ?deftipo ?tipo))
        then (retract ?id)
    )
)

(defrule eliminar-tipos-descartado
    (tipo-descartado ?deftipo)
    ?id <- (pokemon (tipo ?tipo))
    =>
    (if (eq ?deftipo ?tipo)
        then (retract ?id)
    )
)

(defrule determinar-tipo ""
    (not (tipo-definido ?))
    (pokemon (tipo ?tipo))
    (not (terminado ?))
    =>
    (if (yes-or-no-p (str-cat "Es de tipo " ?tipo "?(Si/No)?"))
    then 
    (assert (tipo-definido ?tipo))
    else (assert (tipo-descartado ?tipo))) 
)

(defrule eliminar-clase-definida
    (clase-definido ?defclase)
    ?id <- (pokemon (clase ?clase))
    =>
    (if (not(eq ?defclase ?clase))
        then (retract ?id)
    )
)

(defrule eliminar-clase-descartada
    (clase-descartado ?defclase)
    ?id <- (pokemon (clase ?clase))
    =>
    (if (eq ?defclase ?clase)
        then (retract ?id)
    )
)

(defrule determinar-clase ""
    (not (clase-definido ?))
    (not (terminado ?))
    (pokemon (clase ?clase))
    =>
    (if (yes-or-no-p (str-cat "Es un " ?clase "?(Si/No)?"))
    then (assert (clase-definido ?clase))
    else (assert (clase-descartado ?clase)))
)


(defrule eliminar-categoria-definida
    (categoria-definido ?defcategoria)
    ?id <- (pokemon (categoria ?categoria))
    =>
    (if (not(eq ?defcategoria ?categoria))
        then (retract ?id)
    )
)

(defrule eliminar-categoria-descartada
    (categoria-descartado ?defcategoria)
    ?id <- (pokemon (categoria ?categoria))
    =>
    (if (eq ?defcategoria ?categoria)
        then (retract ?id)
    )
)

(defrule determinar-categoria ""
    (not (categoria-definido ?))
    (not (terminado ?))
    (pokemon (categoria ?categoria))
    =>
    (if (yes-or-no-p (str-cat "Es un " ?categoria "?(Si/No)?"))
    then (assert (categoria-definido ?categoria))
    else (assert (categoria-descartado ?categoria)))
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