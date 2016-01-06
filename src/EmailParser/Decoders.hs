module EmailParser.Decoders (qp_dec) where

import Data.List
import Foreign
import Foreign.C.Types
import System.IO.Unsafe as U
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

foreign import ccall "static qp.h qp_dec_c"
    c_qp_dec :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

castEnum :: (Enum a, Enum b) => a -> b
castEnum = toEnum . fromEnum

qp_dec' :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) (BS.ByteString, BS.ByteString)
qp_dec' bs = U.unsafePerformIO $ BSU.unsafeUseAsCStringLen bs $ \ (inBuf, inLen) -> do
    outBuf <- mallocBytes inLen
    alloca $ \ pOutLen ->
        alloca $ \ pRemBuf ->
            alloca $ \ pRemLen -> do
                poke pOutLen (castEnum inLen)
                r <- c_qp_dec (castPtr inBuf) (castEnum inLen) outBuf pOutLen pRemBuf pRemLen
                outLen <- peek pOutLen
                newOutBuf <- reallocBytes outBuf (castEnum outLen)
                remBuf <- peek pRemBuf
                remLen <- peek pRemLen
                remBs <- BS.packCStringLen (castPtr remBuf, castEnum remLen)
                outBs <- BSU.unsafePackCStringFinalizer newOutBuf (castEnum outLen) (free newOutBuf)
                if r == 0
                    then return $ Right (outBs, remBs)
                    else return $ Left (outBs, remBs)

qp_dec :: BS.ByteString -> Either (BS.ByteString, BS.ByteString) BS.ByteString
qp_dec bs = case qp_dec' bs of
    Right a@(d, r) -> if BS.null r
            then Right d
            else Left a
    Left a -> Left a
