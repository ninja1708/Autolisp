(setq dvk1m '())  ;; Lista globalna do przechowywania długości rur
(setq dvk '())
(setq srs '())
(setq hdbe '())
(defun warstwy
	(if (eq switch 1)
		(progn
		  ;; Pobieranie pierwotnej warstwy
  		(setq oldLayer (getvar "clayer"))
  
  		;; Ustawienie warstwy na ".rura"
  		(setq newLayer ".rura")
  		(princ "Zmiana warstwy na .rura")
  		(setvar "clayer" newLayer)
 			)
		(progn
	  
  		;; Przywracanie pierwotnej warstwy
		  (princ "\nZmiana warstwy na standardową")
		  (setvar "clayer" oldLayer)
  
  		(princ "\nLinia została utworzona.")
   		)
	)
) 
(defun C:r1 () ;; Rura DVK 1m
	(warstwy 1)
  
  ;; Pobieranie pierwszego punktu od użytkownika
  (setq pt1 (getpoint "Podaj środek rury: "))
  (setq pt2 (getpoint "Podaj punkt na osi: "))
  ;; Ustawienie odległości na 1 m
  (setq odl 1.0)
  
  ;; Dodanie wartości do listy lenght_list1
  (setq dvk1m (append dvk1m (list odl)))
  (setq pol (/ odl 2))
  
  ;; Obliczanie wektora kierunkowego
  (setq dx (- (car pt2) (car pt1)))
  (setq dy (- (cadr pt2) (cadr pt1)))
  (setq dz (- (caddr pt2) (caddr pt1)))
  
  ;; Obliczanie Długiści wektora kierunkowego
  (setq length (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
  
  ;; Obliczanie współczynnika kierunkowego
  (setq ux (/ dx length))
  (setq uy (/ dy length))
  (setq uz (/ dz length))
  
  ;; Obliczanie nowego punktu końcowego
  (setq newPt1 (list (+ (car pt1) (* pol ux))
                     (+ (cadr pt1) (* pol uy))
                     (+ (caddr pt1) (* pol uz))))
  (setq newPt2 (list (- (car pt1) (* pol ux))
                     (- (cadr pt1) (* pol uy))
                     (- (caddr pt1) (* pol uz))))
  
  ;; Pobieranie punktu przesunięcia od użytkownika
  (setq offsetPoint (getpoint "Podaj punkt, aby określić kierunek przesunięcia: "))
  
  ;; Obliczanie wektora linii pt1-pt2
  (setq lineVec (list (- (car newPt2) (car newPt1))
                      (- (cadr newPt2) (cadr newPt1))
                      (- (caddr newPt2) (caddr newPt1))))
  ;; Obliczanie długości wektora linii
  (setq lineLength (sqrt (+ (* (car lineVec) (car lineVec))
                            (* (cadr lineVec) (cadr lineVec))
                            (* (caddr lineVec) (caddr lineVec)))))
  ;; Obliczanie jednostkowego wektora linii
  (setq lineUnitVec (list (/ (car lineVec) lineLength)
                          (/ (cadr lineVec) lineLength)
                          (/ (caddr lineVec) lineLength)))
 
  ;; Obliczanie wektora prostopadłego do linii pt1-pt2 w płaszczyźnie XY
  (setq perpUnitVec (list (- (cadr lineUnitVec)) (car lineUnitVec) 0))
  
  ;; Obliczanie wektora przesunięcia w kierunku offsetPoint
  (setq offsetVec (list (- (car offsetPoint) (car newPt2))
                        (- (cadr offsetPoint) (cadr newPt2))
                        (- (caddr offsetPoint) (caddr newPt2))))
 
  ;; Obliczanie długości wektora przesunięcia
  (setq offsetLength (sqrt (+ (* (car offsetVec) (car offsetVec))
                              (* (cadr offsetVec) (cadr offsetVec))
                              (* (caddr offsetVec) (caddr offsetVec)))))
  
 
  ;; Obliczanie jednostkowego wektora przesunięcia
  (setq offsetUnitVec (list (/ (car offsetVec) offsetLength)
                            (/ (cadr offsetVec) offsetLength)
                            (/ (caddr offsetVec) offsetLength)))
  
  ;; Obliczanie iloczynu skalarnego między wektorem prostopadłym a wektorem przesunięcia
  (setq dotProduct (+ (* (car perpUnitVec) (car offsetUnitVec))
                      (* (cadr perpUnitVec) (cadr offsetUnitVec))
                      (* (caddr perpUnitVec) (caddr offsetUnitVec))))
  
  ;; Określanie kierunku przesunięcia na podstawie znaku iloczynu skalarnego
  (setq direction (if (>= dotProduct 0) 1 -1))
  
  ;; Przesuwanie punktów o 0.2 jednostki w kierunku prostopadłym do linii pt1-pt2
  (setq offsetDist 0.2) ;; odległość przesunięcia
  (setq newPt1 (list (+ (car newPt1) (* offsetDist (car perpUnitVec) direction))
                     (+ (cadr newPt1) (* offsetDist (cadr perpUnitVec) direction))
                     (+ (caddr newPt1) (* offsetDist (caddr perpUnitVec) direction))))
  
  (setq newPt2 (list (+ (car newPt2) (* offsetDist (car perpUnitVec) direction))
                     (+ (cadr newPt2) (* offsetDist (cadr perpUnitVec) direction))
                     (+ (caddr newPt2) (* offsetDist (caddr perpUnitVec) direction))))
  
  ;; Tworzenie przesuniętej linii
  (entmakex (list '(0 . "LINE")
                  (cons 10 newPt1)
                  (cons 11 newPt2)
                  (cons 62 5 ) ;; Kolor niebieski
            )
  )
  
  (warstwy 0)

)

(defun C:r2_DVK () ;; Rura DVK powyżej 1m
  ;; Pobieranie pierwotnej warstwy
  (setq oldLayer (getvar "clayer"))
  
  ;; Ustawienie warstwy na ".rura"
  (setq newLayer ".rura") 
  ;; Ustawienie warstwy na nową
  (setvar "clayer" newLayer) 
  
  ;; Pobieranie pierwszego punktu od użytkownika
  (setq pt1 (getpoint "Podaj początek rury: "))
  
  ;; Pobieranie drugiego punktu od użytkownika
  (setq pt2 (getpoint "Podaj koniec rury: "))
  
  ;; Pobieranie punktu przesunięcia od użytkownika
  (setq offsetPoint (getpoint "Podaj punkt, aby określić kierunek przesunięcia: "))
  
  ;; Obliczanie wektora linii pt1-pt2
  (setq lineVec (list (- (car pt2) (car pt1))
                      (- (cadr pt2) (cadr pt1))
                      (- (caddr pt2) (caddr pt1))
                )
  )
  
  ;; Obliczanie długości wektora linii
  (setq lineLength (sqrt (+ (* (car lineVec) (car lineVec))
                            (* (cadr lineVec) (cadr lineVec))
                            (* (caddr lineVec) (caddr lineVec)))))
  
  ;; Obliczanie jednostkowego wektora linii
  (setq lineUnitVec (list (/ (car lineVec) lineLength)
                          (/ (cadr lineVec) lineLength)
                          (/ (caddr lineVec) lineLength)
                    )
  )
  
  ;; Obliczanie wektora prostopadłego do linii pt1-pt2 w płaszczyźnie XY
  (setq perpUnitVec (list (- (cadr lineUnitVec)) (car lineUnitVec) 0))
  
  ;; Obliczanie wektora przesunięcia w kierunku offsetPoint
  (setq offsetVec (list (- (car offsetPoint) (car pt2))
                        (- (cadr offsetPoint) (cadr pt2))
                        (- (caddr offsetPoint) (caddr pt2))))
  
  ;; Obliczanie długości wektora przesunięcia
  (setq offsetLength (sqrt (+ (* (car offsetVec) (car offsetVec))
                              (* (cadr offsetVec) (cadr offsetVec))
                              (* (caddr offsetVec) (caddr offsetVec)))
                     )
  )
  
  ;; Obliczanie jednostkowego wektora przesunięcia
  (setq offsetUnitVec (list (/ (car offsetVec) offsetLength)
                            (/ (cadr offsetVec) offsetLength)
                            (/ (caddr offsetVec) offsetLength))
  )
  
  ;; Obliczanie iloczynu skalarnego między wektorem prostopadłym a wektorem przesunięcia
  (setq dotProduct (+ (* (car perpUnitVec) (car offsetUnitVec))
                      (* (cadr perpUnitVec) (cadr offsetUnitVec))
                      (* (caddr perpUnitVec) (caddr offsetUnitVec))
                   )
  )
  
  ;; Określanie kierunku przesunięcia na podstawie znaku iloczynu skalarnego
  (setq direction (if (>= dotProduct 0) 1 -1))
  
  ;; Przesuwanie punktów o 0.2 jednostki w kierunku prostopadłym do linii pt1-pt2
  (setq offsetDist 0.2)  ;; odległość przesunięcia
  (setq newPt1 (list (+ (car pt1) (* offsetDist (car perpUnitVec) direction))
                     (+ (cadr pt1) (* offsetDist (cadr perpUnitVec) direction))
                     (+ (caddr pt1) (* offsetDist (caddr perpUnitVec) direction))
               )
  )
  
  (setq newPt2 (list (+ (car pt2) (* offsetDist (car perpUnitVec) direction))
                     (+ (cadr pt2) (* offsetDist (cadr perpUnitVec) direction))
                     (+ (caddr pt2) (* offsetDist (caddr perpUnitVec) direction))
               )
  )
  
  ;; Tworzenie przesuniętej linii
  (entmakex (list '(0 . "LINE")
                  (cons 10 newPt1)
                  (cons 11 newPt2)
                  (cons 62 5 ) ;; Kolor niebieski
                   
            )
  )

  ;; Obliczanie długości przesuniętej linii
  (setq odl1 (distance newPt1 newPt2))

  ;; Obliczenie modulo długości
  (setq mod (rem odl1 1))
  
  ; Zaokrąglenie długości
  (setq fixes (fix odl1))
  (if (>= mod 0.5)
    (setq odl1 (+ fixes 1))
    (setq odl1 fixes)
  )
  
  ;; Dodanie wartości do listy lenght_list2
  (setq dvk (append dvk (list odl1)))
  
  ;;Wypisanie długości
  (setq odl1 (rtos odl1 2 0))
  
  ;; połączenie długości z metrami
  (setq tekst (strcat odl1 ",0m"))
  
  ;; Obliczanie kąta linii newPt1-newPt2
  (setq dx (- (car newPt2) (car newPt1)))
  (setq dy (- (cadr newPt2) (cadr newPt1)))
  (setq ang (atan dy dx))
  (setq pt3 (- (car newPt1) (car newPt2)) )
  
  ;; Ustawienie napisu
  (setq forward 0)
  (setq offset 0.2)
  
  ;: Sprawdzanie orientacji w przestrzenie X Y do poprawnego wyświetlania napisu
  (if (> pt3 0) (if (= direction -1)
                (setq offset (setq offset 0.2) || (setq forward 6) )
                (setq offset (setq offset 1) || (setq forward 6))

                )
       
  )
  (if (< pt3 0) (if (= direction -1)
                (setq offset (setq offset 1) )
                (setq offset (setq offset 0.2))
                )
  )

  ;; obliczanie punktu środkowego
  (setq textPt (list (/ (+ (car newPt1) (car newPt2)) 2)
                    (/ (+ (cadr newPt1) (cadr newPt2)) 2)
                    (/ (+ (caddr newPt1) (caddr newPt2)) 2)
               )
  )
  (setq textPt (list (+ (car textPt) (* offset (car perpUnitVec) direction))
                     (+ (cadr textPt) (* offset (cadr perpUnitVec) direction))
                     (+ (caddr textPt) (* offset (caddr perpUnitVec) direction))
               )
  )
  
  (setq newLayer ".opisy rur") 
  ;; Ustawienie warstwy na nową
  (setvar "clayer" newLayer) 
  ;; Tworzenie tekstu
  (entmakex (list '(0 . "TEXT")
                  (cons 10 textPt)
                  (cons 40 0.8)  ;; Wysokość tekstu, można zmienić w razie potrzeby
                  (cons 1 tekst)
                  (cons 50 ang)
                  (cons 41 0.8) ;; szerokośc tekstu
                  (cons 7 "Arial Narrow")
                  (cons 71 forward) ;; odwrócenie tekstu a także napisanie go wstecz
            )
  )
  
  ;; Przywrócenie pierwotnej warstwy
  (setvar "clayer" oldLayer)
)

(defun C:r3_SRS () ;; Rura srs błękitna
  ;; Pobieranie pierwotnej warstwy
  (setq oldLayer (getvar "clayer"))
  
  ;; Ustawienie warstwy na ".rura"
  (setq newLayer ".rura") 
  ;; Ustawienie warstwy na nową
  (setvar "clayer" newLayer) 
  
  ;; Pobieranie pierwszego punktu od użytkownika
  (setq pt1 (getpoint "Podaj początek rury: "))
  
  ;; Pobieranie drugiego punktu od użytkownika
  (setq pt2 (getpoint "Podaj koniec rury: "))
  
  ;; Pobieranie punktu przesunięcia od użytkownika
  (setq offsetPoint (getpoint "Podaj punkt, aby określić kierunek przesunięcia: "))
  
  ;; Obliczanie wektora linii pt1-pt2
  (setq lineVec (list (- (car pt2) (car pt1))
                      (- (cadr pt2) (cadr pt1))
                      (- (caddr pt2) (caddr pt1))
                )
  )
  
  ;; Obliczanie długości wektora linii
  (setq lineLength (sqrt (+ (* (car lineVec) (car lineVec))
                            (* (cadr lineVec) (cadr lineVec))
                            (* (caddr lineVec) (caddr lineVec)))))
  
  ;; Obliczanie jednostkowego wektora linii
  (setq lineUnitVec (list (/ (car lineVec) lineLength)
                          (/ (cadr lineVec) lineLength)
                          (/ (caddr lineVec) lineLength)
                    )
  )
  
  ;; Obliczanie wektora prostopadłego do linii pt1-pt2 w płaszczyźnie XY
  (setq perpUnitVec (list (- (cadr lineUnitVec)) (car lineUnitVec) 0))
  
  ;; Obliczanie wektora przesunięcia w kierunku offsetPoint
  (setq offsetVec (list (- (car offsetPoint) (car pt2))
                        (- (cadr offsetPoint) (cadr pt2))
                        (- (caddr offsetPoint) (caddr pt2))))
  
  ;; Obliczanie długości wektora przesunięcia
  (setq offsetLength (sqrt (+ (* (car offsetVec) (car offsetVec))
                              (* (cadr offsetVec) (cadr offsetVec))
                              (* (caddr offsetVec) (caddr offsetVec)))
                     )
  )
  
  ;; Obliczanie jednostkowego wektora przesunięcia
  (setq offsetUnitVec (list (/ (car offsetVec) offsetLength)
                            (/ (cadr offsetVec) offsetLength)
                            (/ (caddr offsetVec) offsetLength))
  )
  
  ;; Obliczanie iloczynu skalarnego między wektorem prostopadłym a wektorem przesunięcia
  (setq dotProduct (+ (* (car perpUnitVec) (car offsetUnitVec))
                      (* (cadr perpUnitVec) (cadr offsetUnitVec))
                      (* (caddr perpUnitVec) (caddr offsetUnitVec))
                   )
  )
  
  ;; Określanie kierunku przesunięcia na podstawie znaku iloczynu skalarnego
  (setq direction (if (>= dotProduct 0) 1 -1))
  
  ;; Przesuwanie punktów o 0.2 jednostki w kierunku prostopadłym do linii pt1-pt2
  (setq offsetDist 0.2)  ;; odległość przesunięcia
  (setq newPt1 (list (+ (car pt1) (* offsetDist (car perpUnitVec) direction))
                     (+ (cadr pt1) (* offsetDist (cadr perpUnitVec) direction))
                     (+ (caddr pt1) (* offsetDist (caddr perpUnitVec) direction))
               )
  )
  
  (setq newPt2 (list (+ (car pt2) (* offsetDist (car perpUnitVec) direction))
                     (+ (cadr pt2) (* offsetDist (cadr perpUnitVec) direction))
                     (+ (caddr pt2) (* offsetDist (caddr perpUnitVec) direction))
               )
  )
  
  ;; Tworzenie przesuniętej linii
  (entmakex (list '(0 . "LINE")
                  (cons 10 newPt1)
                  (cons 11 newPt2)
                  (cons 62 130 ) ;; Kolor błękitny
            )
  )

  ;; Obliczanie długości przesuniętej linii
  (setq odl1 (distance newPt1 newPt2))

  ;; Obliczenie modulo długości
  (setq mod (rem odl1 1))
  
  ; Zaokrąglenie długości
  (setq fixes (fix odl1))
  (if (>= mod 0.5)
    (setq odl1 (+ fixes 1))
    (setq odl1 fixes)
  )
 
  ;; Dodanie wartości do listy lenght_list3
  (setq srs (append srs (list odl1)))

  ;;Wypisanie długości
  (setq odl1 (rtos odl1 2 0))
  
  ;; połączenie długości z metrami
  (setq tekst (strcat odl1 ",0m"))
  
  ;; Obliczanie kąta linii newPt1-newPt2
  (setq dx (- (car newPt2) (car newPt1)))
  (setq dy (- (cadr newPt2) (cadr newPt1)))
  (setq ang (atan dy dx))
  (setq pt3 (- (car newPt1) (car newPt2)) )
  
  ;; Ustawienie napisu
  (setq forward 0)
  (setq offset 0.2)
  
  ;: Sprawdzanie orientacji w przestrzenie X Y do poprawnego wyświetlania napisu
  (if (> pt3 0) (if (= direction -1)
                (setq offset (setq offset 0.2) || (setq forward 6) )
                (setq offset (setq offset 1) || (setq forward 6))

                )
       
  )
  (if (< pt3 0) (if (= direction -1)
                (setq offset (setq offset 1) )
                (setq offset (setq offset 0.2))
                )
  )

  ;; obliczanie punktu środkowego
  (setq textPt (list (/ (+ (car newPt1) (car newPt2)) 2)
                    (/ (+ (cadr newPt1) (cadr newPt2)) 2)
                    (/ (+ (caddr newPt1) (caddr newPt2)) 2)
               )
  )
  (setq textPt (list (+ (car textPt) (* offset (car perpUnitVec) direction))
                     (+ (cadr textPt) (* offset (cadr perpUnitVec) direction))
                     (+ (caddr textPt) (* offset (caddr perpUnitVec) direction))
               )
  )
  
  (setq newLayer ".opisy rur") 
  ;; Ustawienie warstwy na nową
  (setvar "clayer" newLayer) 
  ;; Tworzenie tekstu
  (entmakex (list '(0 . "TEXT")
                  (cons 10 textPt)
                  (cons 40 0.8)  ;; Wysokość tekstu, można zmienić w razie potrzeby
                  (cons 1 tekst)
                  (cons 50 ang)
                  (cons 41 0.8) ;; szerokośc tekstu
                  (cons 7 "Arial Narrow")
                  (cons 62 130 ) ;; Kolor błękitny
                  (cons 71 forward) ;; odwrócenie tekstu a także napisanie go wstecz
            )
  )
  
  ;; Przywrócenie pierwotnej warstwy
  (setvar "clayer" oldLayer)
)

(defun C:r4_HDPE  () ;; Rura HDPE Magenta
  ;; Pobieranie pierwotnej warstwy
  (setq oldLayer (getvar "clayer"))
  
  ;; Ustawienie warstwy na ".rura"
  (setq newLayer ".rura") 
  ;; Ustawienie warstwy na nową
  (setvar "clayer" newLayer) 
  
  ;; Pobieranie pierwszego punktu od użytkownika
  (setq pt1 (getpoint "Podaj początek rury: "))
  
  ;; Pobieranie drugiego punktu od użytkownika
  (setq pt2 (getpoint "Podaj koniec rury: "))
  
  ;; Pobieranie punktu przesunięcia od użytkownika
  (setq offsetPoint (getpoint "Podaj punkt, aby określić kierunek przesunięcia: "))
  
  ;; Obliczanie wektora linii pt1-pt2
  (setq lineVec (list (- (car pt2) (car pt1))
                      (- (cadr pt2) (cadr pt1))
                      (- (caddr pt2) (caddr pt1))
                )
  )
  
  ;; Obliczanie długości wektora linii
  (setq lineLength (sqrt (+ (* (car lineVec) (car lineVec))
                            (* (cadr lineVec) (cadr lineVec))
                            (* (caddr lineVec) (caddr lineVec)))))
  
  ;; Obliczanie jednostkowego wektora linii
  (setq lineUnitVec (list (/ (car lineVec) lineLength)
                          (/ (cadr lineVec) lineLength)
                          (/ (caddr lineVec) lineLength)
                    )
  )
  
  ;; Obliczanie wektora prostopadłego do linii pt1-pt2 w płaszczyźnie XY
  (setq perpUnitVec (list (- (cadr lineUnitVec)) (car lineUnitVec) 0))
  
  ;; Obliczanie wektora przesunięcia w kierunku offsetPoint
  (setq offsetVec (list (- (car offsetPoint) (car pt2))
                        (- (cadr offsetPoint) (cadr pt2))
                        (- (caddr offsetPoint) (caddr pt2))))
  
  ;; Obliczanie długości wektora przesunięcia
  (setq offsetLength (sqrt (+ (* (car offsetVec) (car offsetVec))
                              (* (cadr offsetVec) (cadr offsetVec))
                              (* (caddr offsetVec) (caddr offsetVec)))
                     )
  )
  
  ;; Obliczanie jednostkowego wektora przesunięcia
  (setq offsetUnitVec (list (/ (car offsetVec) offsetLength)
                            (/ (cadr offsetVec) offsetLength)
                            (/ (caddr offsetVec) offsetLength))
  )
  
  ;; Obliczanie iloczynu skalarnego między wektorem prostopadłym a wektorem przesunięcia
  (setq dotProduct (+ (* (car perpUnitVec) (car offsetUnitVec))
                      (* (cadr perpUnitVec) (cadr offsetUnitVec))
                      (* (caddr perpUnitVec) (caddr offsetUnitVec))
                   )
  )
  
  ;; Określanie kierunku przesunięcia na podstawie znaku iloczynu skalarnego
  (setq direction (if (>= dotProduct 0) 1 -1))
  
  ;; Przesuwanie punktów o 0.2 jednostki w kierunku prostopadłym do linii pt1-pt2
  (setq offsetDist 0.2)  ;; odległość przesunięcia
  (setq newPt1 (list (+ (car pt1) (* offsetDist (car perpUnitVec) direction))
                     (+ (cadr pt1) (* offsetDist (cadr perpUnitVec) direction))
                     (+ (caddr pt1) (* offsetDist (caddr perpUnitVec) direction))
               )
  )
  
  (setq newPt2 (list (+ (car pt2) (* offsetDist (car perpUnitVec) direction))
                     (+ (cadr pt2) (* offsetDist (cadr perpUnitVec) direction))
                     (+ (caddr pt2) (* offsetDist (caddr perpUnitVec) direction))
               )
  )
  
  ;; Tworzenie przesuniętej linii
  (entmakex (list '(0 . "LINE")
                  (cons 10 newPt1)
                  (cons 11 newPt2)
                  (cons 62 210 ) ;; Kolor magenta
            )
  )

  ;; Obliczanie długości przesuniętej linii
  (setq odl1 (distance newPt1 newPt2))

  ;; Obliczenie modulo długości
  (setq mod (rem odl1 1))
  
  ; Zaokrąglenie długości
  (setq fixes (fix odl1))
  (if (>= mod 0.5)
    (setq odl1 (+ fixes 1))
    (setq odl1 fixes)
  )
  ;; Dodanie wartości do listy lenght_list4
  (setq hdbe (append hdbe (list odl1)))
  
  ;;Wypisanie długości
  (setq odl1 (rtos odl1 2 0))
  
  ;; połączenie długości z metrami
  (setq tekst (strcat odl1 ",0m"))
  
  ;; Obliczanie kąta linii newPt1-newPt2
  (setq dx (- (car newPt2) (car newPt1)))
  (setq dy (- (cadr newPt2) (cadr newPt1)))
  (setq ang (atan dy dx))
  (setq pt3 (- (car newPt1) (car newPt2)) )
  
  ;; Ustawienie napisu
  (setq forward 0)
  (setq offset 0.2)
  
  ;: Sprawdzanie orientacji w przestrzenie X Y do poprawnego wyświetlania napisu
  (if (> pt3 0) (if (= direction -1)
                (setq offset (setq offset 0.2) || (setq forward 6) )
                (setq offset (setq offset 1) || (setq forward 6))

                )
       
  )
  (if (< pt3 0) (if (= direction -1)
                (setq offset (setq offset 1) )
                (setq offset (setq offset 0.2))
                )
  )

  ;; obliczanie punktu środkowego
  (setq textPt (list (/ (+ (car newPt1) (car newPt2)) 2)
                    (/ (+ (cadr newPt1) (cadr newPt2)) 2)
                    (/ (+ (caddr newPt1) (caddr newPt2)) 2)
               )
  )
  (setq textPt (list (+ (car textPt) (* offset (car perpUnitVec) direction))
                     (+ (cadr textPt) (* offset (cadr perpUnitVec) direction))
                     (+ (caddr textPt) (* offset (caddr perpUnitVec) direction))
               )
  )
  
  (setq newLayer ".opisy rur") 
  ;; Ustawienie warstwy na nową
  (setvar "clayer" newLayer) 
  ;; Tworzenie tekstu
  (entmakex (list '(0 . "TEXT")
                  (cons 10 textPt)
                  (cons 40 0.8)  ;; Wysokość tekstu, można zmienić w razie potrzeby
                  (cons 1 tekst)
                  (cons 50 ang)
                  (cons 41 0.8) ;; szerokośc tekstu
                  (cons 7 "Arial Narrow")
                  (cons 62 210 ) ;; Kolor magenta
                  (cons 71 forward) ;; odwrócenie tekstu a także napisanie go wstecz
            )
  )
  
  ;; Przywrócenie pierwotnej warstwy
  (setvar "clayer" oldLayer)
)




