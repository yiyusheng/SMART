dir <- '/home/yiyusheng/cs'
fileNames <- list.files(dir)
filePath <- file.path(dir,fileNames)

write.table(fileNames,file = '/home/yiyusheng/readDir.txt',quote = F,row.names = F,col.names = F)
write.table(filePath,file = '/home/yiyusheng/readDirPath.txt',quote = F,row.names = F,col.names = F)
