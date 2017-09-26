{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.Api where

import Foreign.C.Types
import Foreign.C.String

foreign import ccall "math.h sin" c_sin :: Double -> Double
foreign import ccall "add"  c_add  :: CInt -> CInt -> CInt
foreign import ccall "two"  c_two  :: CInt
foreign import ccall "moji" c_moji :: CInt -> CString
