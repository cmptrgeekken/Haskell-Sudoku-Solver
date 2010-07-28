> module Sudoku where
> import Hutton
> import List(elemIndex)

Represents Sudoku as a flat array

> solved :: [Integer] -> Bool
> solved board = 0 `notElem` board

> choices :: [Integer] -> [Integer]
> choices board = 
>		{--} [v | v <- [1..9], safe v]
>			where
>				cell = case elemIndex 0 board of
>					Nothing -> -1
>					Just a  ->  a
>				maxVal = 9 {-(floor $ sqrt $ fromIntegral $ length board)-}
>				rowNum a = (a `div` maxVal)
>				colNum a = (a `mod` maxVal)
>				row = [elem | ind <- [0..length board-1],(rowNum ind)==(rowNum cell),elem<-[board!!ind]]
>				col = [elem | ind <- [0..length board-1],(colNum ind)==(colNum cell),elem<-[board!!ind]]
>				boxNum a = (colNum a) `div` 3 + (3*((rowNum a) `div` 3))
>				box = [elem | ind <- [0..length board],(boxNum ind) == (boxNum cell),elem <- [board!!ind]]
>				safe v = 
>					v `notElem` row && 
>					v `notElem` col &&
>					v `notElem` box

> choose :: [Integer] -> Integer -> [Integer]
> choose board val = (leftBoard) ++ [val] ++ (rightBoard)
>	where
>		cell = case elemIndex 0 board of
>			Nothing -> length board - 1
>			Just a  ->  a
>		splitBoard = splitAt cell board
>		leftBoard  = fst splitBoard
>		rightBoard = case snd splitBoard of
>						[] -> []
>						a  -> tail a

> solve :: [Integer] -> [Integer]
> solve puzzle = 
>	case solved puzzle of
>		True -> puzzle
>		otherwise -> 
>			case filter (/=[]) [solve $ choose puzzle r | r <- choices puzzle] of
>				[] -> []
>				puzzle -> (head puzzle)