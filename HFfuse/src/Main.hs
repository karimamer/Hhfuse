module Main where

import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO
import System.Fuse

type HT = ()


main :: IO ()
main = fuseMain gebFSOps defaultExceptionHandler

gebFSOps :: FuseOperations HT
gebFSOps = defaultFuseOps { fuseGetFileStat = gebGetFileStat
                            , fuseOpen        = gebOpen
                            , fuseRead        = gebRead 
                            , fuseOpenDirectory = gebOpenDirectory
                            , fuseReadDirectory = gebReadDirectory
                            , fuseGetFileSystemStats = gebGetFileSystemStats
                            }
gebCheck :: B.ByteString
gebCheck = B.pack "i am in \n"

gebPath :: FilePath
gebPath = "/geb"
dirStat ctx  = FileStat {
                          statEntryType = Directory
                          ,  statFileMode = foldr1 unionFileModes 
                                              [ ownerReadMode
                                              , ownerExecuteMode
                                              , groupReadMode
                                              , groupExecuteMode
                                              , otherReadMode
                                              , otherExecuteMode
                                              ]
                         , statLinkCount = 2
                         , statFileOwner = fuseCtxUserID ctx
                         , statFileGroup = fuseCtxGroupID ctx
                         , statSpecialDeviceID = 0
                         , statFileSize = 4096
                         , statBlocks = 1
                         , statAccessTime = 0
                         , statModificationTime = 0
                         , statStatusChangeTime = 0

                       }

fileStat ctx = FileStat { 
                         statEntryType = RegularFile
                       , statFileMode = foldr1 unionFileModes
                                          [ ownerReadMode 
                                          , groupReadMode
                                          , otherReadMode
                                          ]
                      , statLinkCount = 1
                      , statFileOwner = fuseCtxUserID ctx
                      , statFileGroup = fuseCtxGroupID ctx
                      , statSpecialDeviceID = 0
                      , statFileSize = fromIntegral $ B.length gebCheck
                      , statBlocks = 1
                      , statAccessTime = 0
                      , statModificationTime = 0
                      , statStatusChangeTime = 0
                      }

gebGetFileStat :: FilePath -> IO (Either Errno FileStat)
gebGetFileStat "/" = do
   ctx <- getFuseContext
   return $ Right $ dirStat ctx
gebGetFileStat path | path == gebPath = do
   ctx <- getFuseContext
   return $ Right $ fileStat ctx
gebGetFileStat _ =
    return $ Left eNOENT


gebOpenDirectory "/" = return eOK
gebOpenDirectory _   = return eNOENT



gebReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
gebReadDirectory "/" = do
   ctx <- getFuseContext
   return $ Right [(".",          dirStat  ctx)
                 ,("..",         dirStat  ctx)
                 ,(gebName,    fileStat ctx)
                 ] 
   where (_:gebName) = gebPath
gebDirectory _ = return (Left (eNOENT))


gebOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
gebOpen path mode flags
   | path == gebPath = case mode of
                           ReadOnly -> return (Right ())
                           _        -> return (Left eACCES)
   | otherwise         = return (Left eNOENT)

gebRead :: FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
gebRead path _ byteCount offset
   | path == gebPath =                                                  
        return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) gebCheck
   | otherwise         = return $ Left eNOENT


gebGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)   
gebGetFileSystemStats str =
  return $ Right $ FileSystemStats 
      { fsStatBlockSize = 512
      , fsStatBlockCount = 1
      , fsStatBlocksFree = 1
      , fsStatBlocksAvailable = 1
      , fsStatFileCount = 5
      , fsStatFilesFree = 10
      , fsStatMaxNameLength = 255
     }

       
