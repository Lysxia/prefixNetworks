module Numbers where

-- Obtained sequences can be compared with the
-- Online Encyclopedia of Integer Sequences (oeis.org)

import WSO

depths = [1 .. 20]
widths = [1 .. 32]

s00 = map slices00 depths
size00 = map size s00
-- [1,4,9,18,33,58,99,166,275,452,739,1204,1957,3176,5149,8342,13509,21870,
-- 35399,57290]
-- * A192760
width00 = map width s00
-- [2,4,7,12,20,33,54,88,143,232,376,609,986,1596,2583,4180,6764,10945,17710,
-- 28656]
-- * A000071: Fibonacci - 1
--

s2 = map slices2 depths
size2 = map size s2
-- [1,2,5,8,15,22,37,52,83,114,177,240,367,494,749,1004,1515,2026,3049,4072]
-- * A077866
width2 = map width s2
-- [2,3,5,7,11,15,23,31,47,63,95,127,191,255,383,511,767,1023,1535,2047]
-- * A052955

sklanskys = map sklansky widths
sizeSklansky = map size sklanskys
-- [0,1,2,4,5,7,9,12,13,15,17,20,22,25,28,32,33,35,37,40,42,45,48,52,54,57,60,
-- 64,67,71,75,80]
-- * A000788
