module HaskHOL.Core.Basics where
 
import HaskHOL.Core.Kernel
import HaskHOL.Core.State

genVar :: HOLType -> HOL cls thry HOLTerm

stripComb :: HOLTerm -> (HOLTerm, [HOLTerm])