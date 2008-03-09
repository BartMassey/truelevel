--- Copyright (C) 2007 Bart Massey
--- ALL RIGHTS RESERVED
--- Please see the end of this file for license information.

module Main
where

import Data.List
import Data.Maybe
import System.Environment
import System.Console.ParseArgs
import Data.WAVE
import TrueLevelArgs

-- Create overlapping tiles of n samples.  Each tile is a
-- list of n samples together with a list of n + 2m samples
-- (in general) representing the surrounding window.
offset_tile :: Int -> Int -> [a] -> [([a], [a])]
offset_tile n m s =
    one_tile (0, n, m) s
    where
      one_tile (l, c, r) s =
          let target = take c (drop l s)
              (window, l', s') =
                  if l < r
                  then (take (l + c + r) s,  (l + c) `min` (r + c), s)
                  else (take (r + c + r) (drop (l - r) s), l, drop c s)
          in
            if null target
            then []
            else (target, window) : one_tile (l', c, r) s'

avg :: (Fractional a) => [a] -> a
avg l = sum l / fromIntegral (length l)

--- Estimate peak amplitude using rms power
rms :: [Double] -> Double
rms s = 2 * sqrt (2 * sum (map (\x -> x * x) s) / fromIntegral (length s))

--- Estimate peak amplitude as twice highest absolute peak
--- of low-pass
--- XXX fixme: low-pass is moving average for now
peak :: [Double] -> Double
peak s = 2 * (maximum . map abs . map avg . tile 3 2) s

make_window :: Int -> Int -> [Double]
make_window n w =
    let tri = [ fromIntegral i / fromIntegral (w + 1) |
                i <- [ 1 .. w ] ]
        flat = replicate (n - 2 * w) 1
    in tri ++ flat ++ reverse tri

vprod :: [Double] -> [Double] -> [Double]
vprod l1 l2 = map (\(v1, v2) -> v1 * v2) (zip l1 l2)

compress :: Double -> Double -> Double -> Double
          -> (Double, [[Double]]) -> [[Double]]
compress l h l' h' (s, a)  =
    if s <= 0.0001 then
        map (map (\v -> 0)) a
    else if s >= h then
        scale (h' / s)
    else if s <= l then
        scale (l' / s)
    else let s' = l' + (h' - l') * (s - l) / (h - l) in
        scale (s' / s)
    where
      scale v = map (map (v *)) a

ac :: [Double] -> [Double]
ac tick = map (\v -> v - avg tick) tick

score_tick :: ([Double] -> [Double])
           -> ([Double] -> Double)
           -> [Double]
           -> ([[Double]], [[Double]])
           -> (Double, [[Double]])
score_tick bias power window (target, surround) = 
    let powerf = map (power . bias . (vprod window))
        score = maximum (powerf (transpose surround)) in
    (score, target)

main :: IO ()
main = do
    args <- parseArgsIO ArgsComplete argd
    --- get the input
    h <- getArgStdio args OptionInputFile ReadMode
    wav <- hGetWAVE h
    let samples =  map (map sampleToDouble)
                       (waveSamples wav)
    --- group into ticks
    let hd = waveHeader wav
    let rate = waveFrameRate hd
    let tick_ms = fromJust (getArgInt args OptionTickMS)
    let tick_samples = rate * tick_ms `div` 1000
    let window_pct =
            fromJust (getArgDouble args OptionWindowPercent) / 100
    let window_edge = floor ((window_pct / 2) * fromIntegral tick_samples)
    let overlap = case getArgDouble args OptionWindowOverlap of
                    Nothing -> 2 * window_edge
                    Just p -> floor (p * fromIntegral tick_samples / 100)
    let offset_ticks = offset_tile tick_samples overlap samples
    --- score ticks
    let window = make_window tick_samples overlap
    let bias = if gotArg args OptionDCBias
               then id
               else ac
    let power = if gotArg args OptionUseRMS
                then rms
                else peak
    let scored_ticks = map (score_tick bias power window) offset_ticks
    --- scale the input
    let low_limit =
            fromJust (getArgDouble args OptionLowLimit)  / 100
    let high_limit =
            fromJust (getArgDouble args OptionHighLimit)  / 100
    let low_output =
            fromJust (getArgDouble args OptionLowOutput) / 100
    let high_output =
            fromJust (getArgDouble args OptionHighOutput) / 100
    let lev = compress low_limit high_limit low_output high_output
    let scaled = concatMap lev scored_ticks
    --- write the output
    let samples' = map (map doubleToSample) scaled
    let wav' = WAVE { waveHeader = hd,
                      waveSamples = samples' }
    h' <- getArgStdio args OptionOutputFile WriteMode
    hPutWAVE h' wav'


    
--- Redistribution and use in source and binary forms, with or
--- without modification, are permitted provided that the
--- following conditions are met:
---     * Redistributions of source code must retain the above
---       copyright notice, this list of conditions and the following
---       disclaimer.
---     * Redistributions in binary form must reproduce the
---       above copyright notice, this list of conditions and the
---       following disclaimer in the documentation and/or other
---       materials provided with the distribution.
---     * Neither the name of Bart Massey, nor the names
---       of other affiliated organizations, nor the names
---       of other contributors may be used to endorse or promote
---       products derived from this software without specific prior
---       written permission.
--- 
--- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
--- CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
--- INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
--- MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
--- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
--- CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
--- NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
--- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
--- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
--- OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
