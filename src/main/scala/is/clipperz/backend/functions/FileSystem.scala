/*
package is.clipperz.backend.functions

import java.io.File
import java.nio.file.Files

object FileSystem:
  def deleteAllFiles(file: File): Unit =
    if file.isDirectory() then file.listFiles.nn.map(_.nn).foreach(deleteAllFiles(_))
    else Files.deleteIfExists(file.toPath())
*/