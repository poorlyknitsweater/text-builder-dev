module TextBuilderDev.UTF8 where

import TextBuilderDev.Prelude

{-# INLINE unicodeCodePoint #-}
unicodeCodePoint ::
     Int
  -> (Word8 -> a)
  -> (Word8 -> Word8 -> a)
  -> (Word8 -> Word8 -> Word8 -> a)
  -> (Word8 -> Word8 -> Word8 -> Word8 -> a)
  -> a
unicodeCodePoint c case1 case2 case3 case4
  | c < 0x80    = case1 $ fromIntegral c
  | c < 0x800   = case2 (fromIntegral $ 0xc0 .|. (c `shiftR` 6))  (fromIntegral $ 0x80 .|. (0x3f .&. c))
  | c < 0x10000 = case3 (fromIntegral $ 0xe0 .|. (c `shiftR` 12)) (fromIntegral $ 0x80 .|. (0x3f .&. (c `shiftR` 6)))  (fromIntegral $ 0x80 .|. (0x3f .&. c))
  | otherwise   = case4 (fromIntegral $ 0xf0 .|. (c `shiftR` 18)) (fromIntegral $ 0x80 .|. (0x3f .&. (c `shiftR` 12))) (fromIntegral $ 0x80 .|. (0x3f .&. (c `shiftR` 6))) (fromIntegral $ 0x80 .|. (0x3f .&. c))
