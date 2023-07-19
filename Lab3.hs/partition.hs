partition :: String->[[String]]
partition w =  help w 

help :: String->[[String]]
help [x] = [[[x]]]
help (x:xs) =  [(x:head l):(tail l) | l<- help xs] ++[[x]:l | l<- help xs] 


--  | diaxorisths sthn list
--  <- means gets its result, and binds it to 1.Einai san to = 
-- the singular and the plural (x:xs) / the begining and the end of a word 



