data Blist t = Blist { 
	left :: [t],
	elt :: t,
	right :: [t] 
} deriving (Show)

deplacement_gauche :: Blist t -> Blist t
deplacement_gauche blist=
	Blist { left = tail (left blist), elt = bouge, right = bouge:(right blist) }
	where bouge = head (left blist) 

deplacement_droite :: Blist t -> Blist t
deplacement_droite blist=
	Blist { left = bouge:(left blist), elt = bouge, right = tail (right blist) }
	where bouge = head (right blist)	

update :: Blist t -> Blist t
update blist elem=
	Blist { left = left blist, elt = elem, right = right blist}

-- Renvoie un int qui identifiera si il y a une erreur ou pas
acion :: int -> Blist Char -> Blist Int -> Int 
action saut instB mem =
	if saut > 0 then 
		case elt instB of ']' -> action (saut-1) (deplacement_droite instB) mem 
						  '[' -> action (saut+1) (deplacement_droite instB) mem 
					      otherwise -> action saut (deplacement_droite instB) mem
	else if saut < 0 then 
		case elt instB of ']' -> action (saut+1) (deplacement_gauche instB) mem
						  '[' -> action (saut-1) (deplacement_gauche instB) mem
			      		  otherwise -> action (deplacement_gauche instB) mem
	else
		case elt instB of '>' -> lecture (deplacement_droite mem)
			              '<' -> lecture (deplacement_gauche mem)
			              '+' -> lecture (update mem ((elt mem)+1))
			              '-' -> lecture (update mem ((elt mem)-1))
			              '.' -> 
			              ',' -> 
			              '[' -> action 1 (deplacement_droite instB) mem
			              ']' -> action (-1) (deplacement_gauche instB) mem
			              otherwise -> if saut != 0 then "error" else ()
		where lecture = action saut (deplacement_droite instB)

main = putStrLn "Hello World"