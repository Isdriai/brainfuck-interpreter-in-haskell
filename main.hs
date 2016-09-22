import System.Environment

data Blist t = Blist { 
	left :: [t],
	right :: [t] 
} deriving (Show)

--Il en va de l'utilisateur de mettre autant de '[' que de ']' ! Sinon il y aura une boucle infinie vu le fonctionnement de move_gauche
move_gauche :: Blist t -> Blist t
move_gauche blist=
	case left blist of 
		[]-> blist
		_ -> Blist {left = tail (left blist), right = head (left blist):(right blist)}

move_droite :: Blist t -> Blist t
move_droite blist=
	Blist { left = head (right blist):(left blist), right = tail (right blist) }

update :: Blist t -> t -> Blist t
update blist elem=
	Blist { left = left blist, right = elem:tail (right blist)}

action :: int -> Blist Char -> Blist Int -> IO () 
action saut instB mem =
	if saut > 0 then 
		case right instB of 
			']':_ -> action (saut-1) (move_droite instB) mem 
			'[':_ -> action (saut+1) (move_droite instB) mem 
			_ -> action saut (move_droite instB) mem
	else if saut < 0 then 
		case right instB of 
			']':_ -> action (saut+1) (move_gauche instB) mem
			'[':_ -> action (saut-1) (move_gauche instB) mem
			_ -> action (move_gauche instB) mem
	else
		case right instB of 
			'>':_ -> lecture (move_droite mem)
			'<':_ -> lecture (move_gauche mem)
			'+':_ -> lecture (update mem ((valmem)+1))
			'-':_ -> lecture (update mem ((valmem)-1))
			'.':_ -> do putStrLn (show valmem)
					lecture mem
			',':_ -> do arg <- getLine
						lecture (update meme (read arg :: Int))
			'[':_ -> if valmem == 0 
							then action 1 (move_droite instB) mem 
							else lecture mem
			']':_ -> if valmem mem ==0 
							then action (-1) (move_gauche instB) mem 
							else lecture mem
			[] -> putStrLn "fin programme"
			_ -> lecture mem
		where lecture = action saut (move_droite instB)
			  valmem = head (right mem)

main = do args <- getArgs
		action 0 instruction 
		where instruction = Blist { left = [], right = concat args}
			  memoire = Blist { left = repeat 0, right = repeat 0}