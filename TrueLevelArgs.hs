--- Copyright (C) 2007 Bart Massey
--- ALL RIGHTS RESERVED
--- Please see the end of this file for license information.

module TrueLevelArgs
where

import ParseArgs

data Options =
    OptionUseRMS |
    OptionDCBias |
    OptionTickMS |
    OptionWindowPercent |
    OptionWindowOverlap |
    OptionHighLimit |
    OptionHighOutput |
    OptionLowLimit |
    OptionLowOutput |
    OptionInputFile |
    OptionOutputFile
    deriving (Ord, Eq, Show)

argd :: [ Arg Options ]
argd = [ Arg { argIndex = OptionUseRMS,
               argName = Just "rms",
               argAbbr = Just 'r',
               argData = Nothing,
               argDesc = "Use RMS estimation instead of " ++
                         "direct peak power measurement" },
         Arg { argIndex = OptionDCBias,
               argName = Just "dc-bias",
               argAbbr = Just 'd',
               argData = Nothing,
               argDesc = "Retain DC bias in power measurement" },
         Arg { argIndex = OptionTickMS,
               argName = Just "tick",
               argAbbr = Just 't',
               argData = argDataDefaulted "msecs" ArgtypeInt 10,
               argDesc = "Tick interval" },
         Arg { argIndex = OptionWindowPercent,
               argName = Just "window",
               argAbbr = Just 'w',
               argData = argDataDefaulted "percent" ArgtypeDouble 10,
               argDesc = "Percent of tick at edges of " ++
                         "trapezoidal window" },
         Arg { argIndex = OptionWindowOverlap,
               argName = Just "overlap",
               argAbbr = Just 'o',
               argData = argDataOptional "percent" ArgtypeDouble,
               argDesc = "Percent of tick by which windows overlap" },
         Arg { argIndex = OptionHighLimit,
               argName = Just "high-limit",
               argAbbr = Just 'h',
               argData = argDataDefaulted "percent" ArgtypeDouble 80,
               argDesc = "Input signals above this level " ++
                         "will be limited" },
         Arg { argIndex = OptionLowLimit,
               argName = Just "low-limit",
               argAbbr = Just 'l',
               argData = argDataDefaulted "percent" ArgtypeDouble 0,
               argDesc = "Input signals below this level " ++
                         "will be limited" },
         Arg { argIndex = OptionHighOutput,
               argName = Just "high-output",
               argAbbr = Just 'H',
               argData = argDataDefaulted "percent" ArgtypeDouble 95,
               argDesc = "Output level of high-limited signal" },
         Arg { argIndex = OptionLowOutput,
               argName = Just "low-output",
               argAbbr = Just 'L',
               argData = argDataDefaulted "percent" ArgtypeDouble 0,
               argDesc = "Output level of low-limited signal" },
         Arg { argIndex = OptionInputFile,
               argName = Nothing,
               argAbbr = Nothing,
               argData = argDataOptional "input" ArgtypeString,
               argDesc = "Input file" },
         Arg { argIndex = OptionOutputFile,
               argName = Nothing,
               argAbbr = Nothing,
               argData = argDataOptional "output" ArgtypeString,
               argDesc = "Output file" }]

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
