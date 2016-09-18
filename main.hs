import System.Environment

data Blist t = Blist { 
	left :: [t],
	right :: [t] 
} deriving (Show)

deplacement_gauche :: Blist t -> Blist t
deplacement_gauche blist=
	case left blist of [] -> blist
		    		   otherwise -> 
	Blist { left = tail (left blist), right = head (left blist):(right blist) }

deplacement_droite :: Blist t -> Blist t
deplacement_droite blist=
	Blist { left = head (right blist):(left blist), right = tail (right blist) }

update :: Blist t -> t -> Blist t
update blist elem=
	Blist { left = left blist, right = elem:tail (right blist)}

-- Renvoie un int qui identifiera si il y a une erreur ou pas
acion :: int -> Blist Char -> Blist Int -> IO () 
action saut instB mem =
	if saut > 0 then 
		case right instB of ']':_ -> action (saut-1) (deplacement_droite instB) mem 
						  '[':_ -> action (saut+1) (deplacement_droite instB) mem 
					      otherwise -> action saut (deplacement_droite instB) mem
	else if saut < 0 then 
		case right instB of ']':_ -> action (saut+1) (deplacement_gauche instB) mem
						  '[':_ -> action (saut-1) (deplacement_gauche instB) mem
			      		  otherwise -> action (deplacement_gauche instB) mem
	else
		case right instB of '>':_ -> lecture (deplacement_droite mem)
			              '<':_ -> lecture (deplacement_gauche mem)
			              '+':_ -> lecture (update mem ((elt mem)+1))
			              '-':_ -> lecture (update mem ((elt mem)-1))
			              '.':_ -> do putStrLn (show head (right mem))
			              		   lecture mem
			              ',':_ -> do arg <- getLine
			              		   lecture (update mem (read arg :: Int))
			              '[':_ -> if elt mem = 0 
			              				then action 1 (deplacement_droite instB) mem 
			              				else lecture mem
			              ']':_ -> if elt mem =0 
			              				then action (-1) (deplacement_gauche instB) mem 
			              				else lecture mem
			              [] -> "fin"
			              otherwise -> lecture mem
		where lecture = action saut (deplacement_droite instB)

main = do
	args <- getArgs
	action 0 instruction memoire
	where instruction = Blist { left = [], right = concat args}
		  memoire = Blist { left = repeat 0, elt = 0, right = repeat 0}