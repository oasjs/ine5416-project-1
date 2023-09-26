module Main where

import Board
import Data.Matrix (prettyMatrix)


-- tabuleiro = [
--     [(0,0),(4,1),(3,1),(0,1),(2,1),(5,1),(0,2),(0,3),(0,4),(0,4)],
--     [(0,0),(2,0),(0,1),(0,1),(0,2),(4,2),(2,2),(0,3),(3,3),(0,5)],
--     [(0,0),(0,0),(0,6),(1,6),(4,6),(0,7),(0,8),(1,3),(0,3),(0,5)],
--     [(5,6),(6,6),(0,6),(2,9),(3,9),(0,7),(5,8),(0,10),(0,11),(0,12)],
--     [(0,6),(3,13),(5,13),(0,9),(0,14),(0,7),(3,8),(0,10),(0,11),(0,12)],
--     [(0,13),(0,13),(0,4),(7,14),(0,14),(7,15)(0,8),(5,11),(0,11),(4,11)],
--     [(0,13),(0,13),(5,4),(3,14),(0,15),(2,15)(0,8),(4,8),(0,8),(0,16)],
--     [(0,17),(0,17),(1,4),(5,18),(0,15),(0,15)(0,15),(5,15),(3,16),(0,16)],
--     [(1,17),(3,17),(7,8),(0,18),(0,18),(0,18)(6,18),(0,16),(0,16),(5,16)],
--     [(2,19),(1,19),(0,9),(0,19),(3,18),(0,20)(1,20),(0,20),(3,20),(4,16)]]

main = do

    let boardInfo = [
            [(0,0),(4,1),(3,1),(0,1),(2,1),(5,1),(0,2),(0,3),(0,4),(0,4)],
            [(0,0),(2,0),(0,1),(0,1),(0,2),(4,2),(2,2),(0,3),(3,3),(0,5)],
            [(0,0),(0,0),(0,6),(1,6),(4,6),(0,7),(0,8),(1,3),(0,3),(0,5)],
            [(5,6),(6,6),(0,6),(2,9),(3,9),(0,7),(5,8),(0,10),(0,11),(0,12)],
            [(0,6),(3,13),(5,13),(0,9),(0,14),(0,7),(3,8),(0,10),(0,11),(0,12)],
            [(0,13),(0,13),(0,4),(7,14),(0,14),(7,15),(0,8),(5,11),(0,11),(4,11)],
            [(0,13),(0,13),(5,4),(3,14),(0,15),(2,15),(0,8),(4,8),(0,8),(0,16)],
            [(0,17),(0,17),(1,4),(5,18),(0,15),(0,15),(0,15),(5,15),(3,16),(0,16)],
            [(1,17),(3,17),(7,8),(0,18),(0,18),(0,18),(6,18),(0,16),(0,16),(5,16)],
            [(2,19),(1,19),(0,9),(0,19),(3,18),(0,20),(1,20),(0,20),(3,20),(4,16)]]

    let board = makeBoard boardInfo

    putStrLn "Hello, what is your name?"